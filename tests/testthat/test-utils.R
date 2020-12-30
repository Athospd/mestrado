test_that("image_tensors_to_tbl works", {
  # 3 channels
  img_torch <- torch::torch_tensor(1:prod(2*3*4*5))$reshape(c(2,3,4,5))
  img_df <- image_tensors_to_tbl(img_torch)
  
  expect_equal(dim(img_df), c(2*4*5, 3+3))
  expect_equal(names(img_df), c("i", "y", "x", "c1", "c2", "c3"))
  
  # bw
  img_bw_torch <- torch::torch_tensor(1:prod(4*5))$reshape(c(4,5))
  img_df <- image_tensors_to_tbl(img_bw_torch)
  
  expect_equal(dim(img_df), c(1*4*5, 1+3))
  expect_equal(names(img_df), c("i", "y", "x", "c1"))
})

test_that("ggpixelgrid works", {
  # 3 channels
  img_torch <- torchaudio::functional_create_dct(100, 100, norm = 'ortho')
  img_torch <- img_torch - min(img_torch)/(max(img_torch) - min(img_torch))
  img_df <- image_tensors_to_tbl(img_torch)
  
  gg <- img_df %>% ggpixelgrid(label = paste0("img ", i), grid_colour = "orange", grid_tickness = 0)
  expect_equal(class(gg), c("gg", "ggplot"))
})
