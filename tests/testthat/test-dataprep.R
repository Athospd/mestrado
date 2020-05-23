library(testthat)
library(mestrado)
library(tuneR)

mp3_dir <- system.file("mp3_sample", package = "mestrado")
mp3_files <- list.files(mp3_dir, full.names = TRUE)

temp_dir <- tempdir()

test_that("tidy_audio works", {
  wavs <- tidy_audio(mp3_files, temp_dir)
  wavs <- list.files(temp_dir, full.names = TRUE, pattern = "wav")
  new_audio_file <- gsub("mp3$", "wav", mp3_files)
  new_audio_file <- gsub("^.*/", paste0(temp_dir, "/"), new_audio_file)

  expect_equal(new_audio_file, wavs)
})