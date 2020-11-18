# torch -------------------------------------------------------------------

# x <- array(c(1:27), dim = c(3,3,3))
# x_t <- torch::torch_tensor(x)
# bound <- torch::torch_cat(list(x_t, x_t), dim = 1L)
library(torch)
torch::torch_manual_seed(1)

# dados
N <- 1000
X <- torch::torch_randn(N, 10)
b_real <- 1.5
W_real <- torch::torch_rand(10, 1)
prob <- linkinv(torch::torch_mm(X, W_real) + b_real)
y <- prob$bernoulli()

# modelo
linkinv <- torch::nnf_sigmoid
W <- torch::torch_zeros(ncol(X), 1, requires_grad = TRUE)
b <- torch::torch_zeros(1, requires_grad = TRUE)
torch_loss <- function(y, mu) torch_mean(-y * torch_log(mu) - (1 - y) * torch_log(1 - mu))

# loss_grads <- torch::autograd_grad(perda, W, create_graph = TRUE)
# # grad2 <-
#
# for (idx in seq_along(W)) {
#   torch::autograd_grad(loss_grads[[1]][idx], W[idx], create_graph = TRUE)
# }

alpha <- .5

usar_optimizer <- TRUE

if (!usar_optimizer) {
  ## Sem optimizer
  for(i in 1:1000) {
    # calcular a loss
    eta <- torch_mm(X, W) + b
    mu <- linkinv(eta)
    perda <- torch_loss(y, mu)
    perda$retain_grad()
    # update
    perda$backward()
    with_no_grad({
      W$sub_(alpha * W$grad)
      b$sub_(alpha * b$grad)
      W$grad$zero_()
      b$grad$zero_()
    })
  }
} else {
  ## com optimizer
  optim <- torch::optim_sgd(params = list(W, b), lr = alpha)
  for(i in 1:100) {
    # calcular a loss
    eta <- torch::torch_mm(X, W) + b
    mu <- linkinv(eta)
    perda <- torch_loss(y, mu)
    optim$zero_grad()
    # update
    perda$backward()
    optim$step()
  }
}


W
W_real

b
b_real

# comparando
modelo <- glm.fit(
  cbind(1, torch::as_array(X)), torch::as_array(y),
  family = binomial()
)
modelo$coefficients
