library(mestrado)
library(testthat)


annotation_dir <-  system.file("annotations", package = "mestrado")
annotation_tibble <- tidy_annotations(annotation_dir)

test_that("tidy_annotations works", {
  expect_equal(class(annotation_tibble), c("tbl_df", "tbl", "data.frame"))  
  expect_error(tidy_annotations())
  expect_equal(dim(annotation_tibble), c(25, 5))
})


test_that("retrieve_labels_for_one_dir works", {
  
  slices_dir <-  system.file("wav_sample_slices_1000ms", package = "mestrado")
  slices_200ms_labels <- retrieve_labels_for_one_dir(
    slices_dir, 
    annotation_tibble, 
    pattern = "Glaucidium|Megascops-atricapilla"
  )
  expect_equal(class(slices_200ms_labels), c("tbl_df", "tbl", "data.frame"))
  expect_length(slices_200ms_labels$label, 42)
  expect_false(is.null(slices_200ms_labels$label))
  expect_equal(unique(slices_200ms_labels$label), c("Glaucidium-minutissimum", "unknown", "Megascops-atricapilla"))
})