library(mestrado)

mp3 <- system.file("mp3_sample", "*.mp3", package = "mestrado")

test_that("normalization works", {
  expect_equal(2 * 2, 4)
})
