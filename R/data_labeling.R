
#' Tidy Annotations data.frame
#'
#' Build the data.frame from a directory with the annotations made with \code{\link[wavesurfer]{annotator_app()}}.
#'
#' @param annotations_dir atomic character. Path to the annotations directory.
#'
#' @return
#' @export
#'
#' @examples
#' library(mestrado)
#' annotation_dir <-  system.file("annotations", package = "mestrado")
#' annotation_tibble <- tidy_annotations(annotation_dir)
tidy_annotations <- function(annotations_dir) {
  list.files(annotations_dir, full.names = TRUE) %>%
  purrr::map_dfr(readRDS) %>%
  tidyr::drop_na() %>%
  tibble::as_tibble() 
}




