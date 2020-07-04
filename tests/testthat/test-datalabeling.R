library(mestrado)
library(testthat)


annotations_dir <-  system.file("annotations", package = "mestrado")
annotations_tibble <- tidy_annotations(annotations_dir)

test_that("tidy_annotations works", {
  expect_equal(class(annotations_tibble), c("tbl_df", "tbl", "data.frame"))  
  expect_error(tidy_annotations())
  expect_equal(dim(annotations_tibble), c(25, 5))
})


test_that("label_slices works", {
  
  slices_dir <-  system.file("wav_sample_slices_1000ms", package = "mestrado")
  # com dir
  slices_200ms_labels <- label_slices(
    slices_dir, 
    annotations_dir, 
    pattern = "Glaucidium|Megascops-atricapilla"
  )
  expect_equal(class(slices_200ms_labels), c("tbl_df", "tbl", "data.frame"))
  expect_length(slices_200ms_labels$label, 42)
  expect_false(is.null(slices_200ms_labels$label))
  expect_equal(unique(slices_200ms_labels$label), c("Glaucidium-minutissimum", "unknown", "Megascops-atricapilla"))
  
  # com tibble
  slices_200ms_labels <- label_slices(
    slices_dir, 
    annotations_tibble, 
    pattern = "Glaucidium|Megascops-atricapilla"
  )
  expect_equal(class(slices_200ms_labels), c("tbl_df", "tbl", "data.frame"))
  expect_length(slices_200ms_labels$label, 42)
  expect_false(is.null(slices_200ms_labels$label))
  expect_equal(unique(slices_200ms_labels$label), c("Glaucidium-minutissimum", "unknown", "Megascops-atricapilla"))
})