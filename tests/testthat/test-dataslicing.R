library(testthat)
library(mestrado)

wav_dir <- system.file("wav_sample", package = "mestrado")
wav_files <- list.files(wav_dir)

temp_dir <- tempdir()

test_that("slice_one_wav_sequencially works", {
  slices_dir <- slice_one_wav_sequencially(wav_files[1], wav_dir, temp_dir)
  expect_equal(slices_dir, temp_dir)
})


test_that("slice_wavs works", {
  slices_path <- slice_wavs(wav_dir)
  expect_equal(length(slices_path), 1)
  
  slices <- list.files(slices_path)
  expect_equal(length(slices), 245)
})

