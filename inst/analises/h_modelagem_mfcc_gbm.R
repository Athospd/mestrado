# BASEADO EM: https://www.groundai.com/project/end-to-end-environmental-sound-classification-using-a-1d-convolutional-neural-network/1
library(mestrado)
library(torch)
library(torchaudio)
library(torchvision)
library(tidyverse)
library(tidymodels)
library(treesnip)
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
  mfcc_spec <- transform_mfcc(n_mfcc = 13, log_mels = TRUE, device = device)
  tensors <- pad_sequence(tensors)$to(device = device)
  tensors <- mfcc_spec(tensors) %>% 
    nnf_max_pool2d(kernel_size = c(1, 2)) %>% 
    torch_flatten(start_dim = 3L, end_dim = 4L)
  targets <- torch::torch_tensor(unlist(targets))$to(device = device)
  
  return(list(tensors = tensors, targets = targets))
}

batch_size <- 300

device <- torch_device("cpu")

if(device$type == "cuda") {
  num_workers <- 1
  pin_memory <- TRUE
} else {
  num_workers <- 0
  pin_memory <- FALSE
}

bcbr2_train_dl <- dataloader(
  dataset = bcbr2_train, batch_size = batch_size, 
  shuffle = FALSE, collate_fn = collate_fn,
  num_workers = num_workers, pin_memory = pin_memory
)
bcbr2_test_dl <- dataloader(
  dataset = bcbr2_test, batch_size = batch_size, 
  shuffle = FALSE, collate_fn = collate_fn,
  num_workers = num_workers, pin_memory = pin_memory
)

make_df <- function(batch) {
  data <- batch[[1]]$to(device = device)
  target <- batch[[2]]$to(device = device)
  
  as_array(data$squeeze()) %>% as_tibble(.name_repair = "unique") %>%
    janitor::clean_names() %>%
    mutate(target = factor(as_array(target)))
}

safe_make_df <- purrr::safely(make_df)

batches <- enumerate(bcbr2_train_dl)
for(batch_idx in seq_along(batches)) {
  batch <- batches[batch_idx][[1]]
  
  mfccs_train_batch <- safe_make_df(batch)
  
  if(!is.null(mfccs_train_batch$error)) break
  
  if(batch_idx == 1) {
    mfccs_train_df <- mfccs_train_batch$result
  } else {
    mfccs_train_df <- rbind(mfccs_train_df, mfccs_train_batch$result)
  }
}

batches <- enumerate(bcbr2_test_dl)
for(batch_idx in seq_along(batches)) {
  batch <- batches[batch_idx][[1]]
  
  mfccs_test_batch <- safe_make_df(batch)
  
  if(!is.null(mfccs_test_batch$error)) break
  
  if(batch_idx == 1) {
    mfccs_test_df <- mfccs_test_batch$result
  } else {
    mfccs_test_df <- rbind(mfccs_test_df, mfccs_test_batch$result)
  }
}

rm(bcbr2_test_dl)
rm(bcbr2_train_dl)
rm(bcbr2_train)
rm(bcbr2_test)
gc()

# grafico por pixel ---------------------------------------
mfccs_train_df %>%
  select(target, x1:x100) %>%
  sample_n(2000) %>%
  pivot_longer(-target) %>%
  ggplot() +
  stat_ecdf(aes(x = value, colour = target)) +
  facet_wrap(~name, scales = "free_x") +
  theme_void()


# modelagem ----------------------------------------------

# initial split -----------------------------------------------------------
mfccs_df <- bind_rows(
  mfccs_train_df %>% mutate(base = "train"),
  mfccs_test_df %>% mutate(base = "test")
)

ids_train <- which(mfccs_df$base %in% "train")
ids_test <- which(mfccs_df$base %in% "test")

initial_split <- make_splits(list(analysis = ids_train, assessment = ids_test), data = mfccs_df)
initial_split$id <- tibble(id = "Resample1") 

# recipe -----------------------------------------------------------------
recipe <- recipe(target ~ ., training(initial_split)) %>%
  step_corr(all_numeric()) %>%
  step_zv(all_predictors()) %>%
  step_normalize(all_numeric())

# model -------------
spec_lgb <- boost_tree(mtry = 100, min_n = 40, trees = 500, tree_depth = tune(), learn_rate = 0.1, loss_reduction = 0.1, sample_size = 0.7) %>% 
  set_mode("classification") %>% 
  set_engine("lightgbm", nthread = 8)

workflow_lgb <- workflow() %>% 
  add_recipe(recipe) %>% 
  add_model(spec_lgb) 

# reamostragem -------------
set.seed(2)
resamples <- vfold_cv(training(initial_split), v = 5)

# tunagem -------------
tictoc::tic("rf")
tune_lgb <- tune_grid(
  workflow_lgb, 
  resamples = resamples, 
  grid = 20,
  control = control_grid(verbose = TRUE, allow_par = FALSE)
)
tictoc::toc()

autoplot(tune_lgb)
show_best(tune_lgb, "accuracy")

# last fit -------------
best_lgb <- select_best(tune_lgb, "accuracy")
workflow_lgb <- workflow_lgb %>% finalize_workflow(best_lgb)

last_fit_lgb <- last_fit(workflow_lgb, initial_split)

collect_metrics(last_fit_lgb)
predictions <- collect_predictions(last_fit_lgb) 
predictions %>% conf_mat(target, .pred_class)


# guarda ------------------------------------------------------------------
saveRDS(last_fit_lgb, "inst/modelos/mfcc_gbm_1seg.rds")
lgb.

# recarrega ---------------------------------------------------------------
# model <- torch::torch_load("inst/modelos/melspectrogram_resnet18_1seg.pt")

# predicao de uma imagem --------------------------------------------------
# TO DO