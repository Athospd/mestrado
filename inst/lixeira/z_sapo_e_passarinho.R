# CNN com 1 conv2d(3, 3) nas duas imagens puras
library(torch)

imagem_passarinho <- torch_zeros(8,6)
imagem_passarinho[3, 1:3] <- torch_ones(3)
imagem_sapo <- torch_zeros(8,6)
imagem_sapo[6, 1:3] <- torch_ones(3)

# (N, F, T) ---> spectrogramas (numero de imagens, faixas de freq, duracao do audio)
x <- torch_stack(c(imagem_passarinho, imagem_sapo))

x <- x$unsqueeze(2)

torchvision::vision_make_grid(x)

quadrado <- nn_conv2d(in_channels = 1, kernel_size = c(3,3), out_channels = 1, bias = FALSE)
retangulo <- nn_conv2d(in_channels = 1, kernel_size = c(8,1), out_channels = 1, bias = FALSE)
# torch_ones(1,1,3,3,requires_grad = TRUE)
# quadrado$weight[1:3,1:3] <- torch_ones(1,1,3,3)

quadrado(x)
retangulo(x)

quadrado$parameters
retangulo$parameters

optim_quadrado <- optim_adam(quadrado$parameters, lr = 0.01)
optim_retangulo <- optim_adam(retangulo$parameters, lr = 0.01)
loss <- torch::nnf_binary_cross_entropy
y <- c(1,0)

for(i in 1:200) {
  optim_quadrado$zero_grad()
  optim_retangulo$zero_grad()
  pred_quadrado <- quadrado(x) %>% nnf_sigmoid() %>% torch_mean(dim = c(2,3,4))
  pred_retangulo <- retangulo(x) %>% nnf_sigmoid() %>% torch_mean(dim = c(2,3,4))
  l_quadrado <- loss(pred_quadrado, y)
  l_retangulo <- loss(pred_retangulo, y)
  l_quadrado$backward()
  l_retangulo$backward()
  optim_quadrado$step()
  optim_retangulo$step()
}

quadrado$parameters$weight$squeeze(1)$squeeze(1) %>% as.matrix() %>% round(3)
retangulo$parameters$weight$squeeze(1)$squeeze(1) %>% as.matrix() %>% round(3)

quadrado(x) %>% nnf_sigmoid() %>% torch_mean(dim = c(2,3,4))
retangulo(x) %>% nnf_sigmoid() %>% torch_mean(dim = c(2,3,4))
