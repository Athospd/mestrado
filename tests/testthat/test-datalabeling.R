library(mestrado)
library(testthat)


test_that("tidy_annotations works", {
  annotation_dir <-  system.file("annotations", package = "mestrado")
  
  annotation_tibble <- tidy_annotations(annotation_dir)
  
  expect_equal(class(annotation_tibble), c("tbl_df", "tbl", "data.frame"))  
  expect_error(tidy_annotations())
  expect_equal(dim(annotation_tibble), c(25, 5))
})
