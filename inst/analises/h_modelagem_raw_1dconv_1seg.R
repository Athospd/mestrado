# BASEADO EM: https://www.groundai.com/project/end-to-end-environmental-sound-classification-using-a-1d-convolutional-neural-network/1
library(mestrado)
library(torch)
library(torchaudio)
library(purrr)

bcbr2_train <- birdcallbr_dataset("data-raw", 1, download = TRUE, train = TRUE)
bcbr2_test <- birdcallbr_dataset("data-raw", 1, download = TRUE, train = FALSE)

pad_sequence <- function(batch) {
  # Make all tensor in a batch the same length by padding with zeros
  batch <- sapply(batch, function(x) (x$t()))
  batch <- torch::nn_utils_rnn_pad_sequence(batch, batch_first = TRUE, padding_value = 0.)
  return(batch$permute(c(1, 3, 2)))
}

collate_fn <- function(batch) {
  # A list has the form:
  # list of lists: (waveform, slice_id, filepath, sample_rate, label, label_one_hot)
  # Transpose it
  batch <- purrr::transpose(batch)
  tensors <- batch$waveform
  targets <- batch$label_index
  
  # Group the list of tensors into a batched tensor
  tensors <- pad_sequence(tensors)$to(device = device)
  targets <- torch::torch_tensor(unlist(targets))$to(device = device)
  
  return(list(tensors = tensors, targets = targets))
}

batch_size <- 64

device <- torch_device(if (cuda_is_available()) "cuda" else "cpu")

if(device$type == "cuda") {
  num_workers <- 1
  pin_memory <- TRUE
} else {
  num_workers <- 0
  pin_memory <- FALSE
}

bcbr2_train_dl <- dataloader(
  dataset = bcbr2_train, batch_size = batch_size, 
  shuffle = TRUE, collate_fn = collate_fn,
  num_workers = num_workers, pin_memory = pin_memory
)
bcbr2_test_dl <- dataloader(
  dataset = bcbr2_test, batch_size = batch_size, 
  shuffle = FALSE, collate_fn = collate_fn,
  num_workers = num_workers, pin_memory = pin_memory
)

Raw1DNet <- nn_module(
  "Raw1DNet",
  initialize = function() {
    self$conv1 <- nn_conv1d( 1,  16, kernel_size = 64, stride = 2) # (1, 16000) --> (16, 7969)
    self$bn1   <- nn_batch_norm1d(16) 
    self$pool1 <- nn_avg_pool1d(8) # (16, 996)
    self$conv2 <- nn_conv1d(16,  32, kernel_size = 32, stride = 2) # (16, 1996) --> (32, 483)
    self$bn2   <- nn_batch_norm1d(32)
    self$pool2 <- nn_avg_pool1d(8) # (32, 60)
    self$conv3 <- nn_conv1d(32,  64, kernel_size = 16, stride = 2) # (32, 123) --> (64, 23)
    self$bn3   <- nn_batch_norm1d(64)
    self$pool3 <- nn_avg_pool1d(2) # (64, 11)
    self$conv4 <- nn_conv1d(64, 128, kernel_size =  2, stride = 1) # (64, 11) --> (128, 10)
    self$bn4   <- nn_batch_norm1d(128)
    self$pool4 <- nn_avg_pool1d(10)
    self$lin1 <- nn_linear(128, 64)
    self$bn5   <- nn_batch_norm1d(64)
    self$lin2 <- nn_linear(64, 10)
    self$bn6   <- nn_batch_norm1d(10)
    self$lin3 <- nn_linear(10, 3)
    self$softmax <- nn_log_softmax(2)
  },
  
  forward = function(x) {
    out <- x %>%
      self$conv1() %>%
      nnf_relu() %>%
      self$bn1() %>%
      self$pool1() %>%
      self$conv2() %>%
      nnf_relu() %>%
      self$bn2() %>% 
      self$pool2() %>%
      self$conv3() %>%
      nnf_relu() %>%
      self$bn3() %>%
      self$pool3() %>%
      self$conv4() %>%
      nnf_relu() %>%
      self$bn4() 
    
    out <- self$pool4(out)$squeeze(3) %>% 
      self$lin1() %>%
      self$bn5() %>%
      nnf_relu() %>%
      self$lin2() %>%
      self$bn6() %>%
      nnf_relu() %>%
      self$lin3() %>%
      self$softmax()
    
    return(out)
  }
)

model <- Raw1DNet()
model$to(device = device)
# model(bcbr2_train_dl$.iter()$.next()$tensors)

str(model$parameters)

count_parameters <- function(model) {
  requires <- purrr::map_lgl(model$parameters, ~.$requires_grad)
  params <- purrr::map_int(model$parameters[requires], ~.$numel())
  sum(params)
}
count_parameters(model)

optimizer <- torch::optim_sgd(model$parameters, lr = 0.005, weight_decay = 0.00001)
scheduler <- torch::lr_step(optimizer, step_size = 10, gamma = 0.1)  # reduce the learning after 20 epochs by a factor of 10
criterion <- nn_nll_loss()

train <- function(model, epoch, log_interval) {
  model$train()
  
  batches <- enumerate(bcbr2_train_dl)
  for(batch_idx in seq_along(batches)) {
    batch <- batches[batch_idx][[1]]
    data <- batch[[1]]$to(device = device)
    target <- batch[[2]]$to(device = device)
    
    # apply transform and model on whole batch directly on device
    output <- model(data)
    # negative log-likelihood for a tensor of size (batch x 1 x n_output)
    loss <- criterion(output, target)$to(device = device)
    
    optimizer$zero_grad()
    loss$backward()
    optimizer$step()
    
    # update progress bar
    pbar$tick(tokens = list(loss = loss$item()))
    
    # record loss
    losses <<- c(losses, loss$item()) 
    if(batch_idx %% log_interval == 0) {
      if(batch_idx != log_interval) dev.off()
      plot(log10(losses), type = "l", col = "royalblue")
    }
  }
}



# count number of correct predictions
number_of_correct <- function(pred, target) {
  return(pred$squeeze()$eq(target)$sum()$item())
}

# find most likely label index for each element in the batch
get_likely_index <- function(tensor) {
  return(tensor$argmax(dim=-1L) + 1L)
}

test <- function(model, epoch) {
  model$eval()
  correct <- 0
  batches <- enumerate(bcbr2_test_dl)
  obs_vs_pred <- data.frame(obs = integer(0), pred = numeric(0))
  for(batch_idx in seq_along(batches)) {
    batch <- batches[batch_idx][[1]]
    data <- batch[[1]]$to(device = device)
    target <- batch[[2]]$to(device = device)
    
    # apply transform and model on whole batch directly on device
    output <- model(data)
    
    pred <- get_likely_index(output)
    correct <- correct + number_of_correct(pred, target)
    obs_vs_pred <- rbind(
      obs_vs_pred, 
      data.frame(
        obs = as.integer(target$to(device = torch_device("cpu"))), 
        pred = as.numeric(pred$to(device = torch_device("cpu")))
      )
    )
    
    # update progress bar
    pbar$tick()
  }
  print(glue::glue("Test Epoch: {epoch}	Accuracy: {correct}/{length(bcbr2_test_dl$dataset)} ({scales::percent(correct / length(bcbr2_test_dl$dataset))})"))
  print(obs_vs_pred %>% dplyr::mutate_all(as.factor) %>% yardstick::conf_mat(obs, pred))
}


log_interval <- 40
n_epoch <- 50

losses <- c()

for(epoch in seq.int(n_epoch)) {
  cat(paste0("Epoch ", epoch, "/", n_epoch, "\n"))
  pbar <- progress::progress_bar$new(total = (length(bcbr2_train_dl) + length(bcbr2_test_dl)), clear = FALSE, width = 90,
                                     incomplete = ".", format = "[:bar] [:current/:total :percent] - ETA: :eta - loss: :loss")
  
  train(model, epoch, log_interval)
  test(model, epoch)
  plot(losses, type = "l", col = "royalblue")
  scheduler$step()
}


# desempenho --------------------------------------------------------------
# Accuracy: 2934/3316 (88%)
#           Truth
# Prediction    1    2    3
#          1  246   39    3
#          2   60 2286  207
#          3    0   73  402


# guarda ------------------------------------------------------------------
# torch::torch_save(model, "inst/modelos/raw_1dconv_1seg.pt")


# recarrega ---------------------------------------------------------------
model <- torch::torch_load("inst/modelos/raw_1dconv_1seg.pt")
model$parameters %>% purrr::walk(function(param) param$requires_grad_(TRUE))


# predicao de uma imagem --------------------------------------------------
# TO DO
