
#' Tidy Annotations data.frame
#'
#' Build the data.frame from a directory with the annotations made with \code{\link[wavesurfer]{annotator_app()}}.
#'
#' @param annotations_dir atomic character. Path to the annotations directory.
#'
#' @return tibble which each row is an annotation.
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
  
  # comentario
}




#' Label Slices of Wave Files
#' 
#' Plug the labels from a set of annotations (made by \code{\link[wavesurfer]{annotator_app}}) in 
#' a dataset of slices (made by \code{\link{slice_wavs}}).
#'
#' @param slices_dir atomic character. Path to dir where the slices are (see \code{\link{slice_wavs}}).
#' @param annotations_dir atomic character or a tibble. If character, path to dir where the annotations 
#' are. If tibble, a tibble created by \code{\link{tidy_annotations}}.
#' @param pattern atomic character. A regex pattern to select files from 'slices_dir'. Useful to filter 
#' out just specifc types of labels (see examples).
#'
#' @return tibble of slices with label column.
#' @export
#'
#' @examples
#' 
#' slices_dir <-  system.file("wav_sample_slices_1000ms", package = "mestrado")
#' annotations_dir <-  system.file("annotations", package = "mestrado")
#' slices_1000ms_labels <- label_slices(
#'   slices_dir, 
#'   annotations_dir, 
#'   pattern = "Glaucidium|Megascops-atricapilla"
#' )
label_slices <- function(slices_dir, annotations, pattern = NULL) {
  
  if(class(annotations)[1] == "character") {
    tidy_annotations <- tidy_annotations(annotations)
  } else if(class(annotations)[1] %in% c("tbl", "data.frame", "tbl_df")) {
    tidy_annotations <- annotations
  }
  
  if(!requireNamespace("IRanges", quietly = TRUE)) {
    usethis::ui_stop("Package IRanges not installed. Run `mestrado::install_iranges()` before continue.")
  }
  slices <- tibble::tibble(slice_id = list.files(slices_dir, pattern = pattern)) %>%
    tidyr::separate(slice_id, c("audio_id", "instant_start", "instant_end"), "@", remove = FALSE, extra = "drop", convert = TRUE) %>%
    dplyr::mutate(audio_id = paste0(audio_id, ".wav"))
  
  slices_labels <- dplyr::left_join(
    slices %>% dplyr::group_by(audio_id) %>% tidyr::nest_legacy(.key = "slices_data"),
    tidy_annotations %>% dplyr::group_by(audio_id) %>% tidyr::nest_legacy(.key = "tidy_annotations_data"),
    by = "audio_id"
  ) %>%
    dplyr::mutate(
      interval_join = purrr::map2(slices_data, tidy_annotations_data, ~{
        if(is.null(.y)) .y <- tibble::tibble(region_id = "a", start = 0, end = 1e6, label = "unknown")
        fuzzyjoin::interval_left_join(
          x = .x %>% dplyr::mutate_if(is.numeric, ~.*1000),
          y = .y %>% dplyr::mutate_if(is.numeric, ~.*1000),
          by = c("instant_start" = "start", "instant_end" =  "end")
        )  %>% dplyr::mutate_if(is.numeric, ~./1000)
      })
    ) %>%
    dplyr::select(
      audio_id,
      interval_join
    ) %>%
    tidyr::unnest_legacy() %>%
    dplyr::mutate(
      label = dplyr::coalesce(label, "unknown")
    ) %>%
    dplyr::select(
      audio_id,
      slice_id,
      label
    ) %>%
    dplyr::distinct(slice_id, .keep_all = TRUE)
  
  return(slices_labels)
}


#' Install IRange
#' 
#' Helper to install IRange from Biocondutor.
#'
#' @return return nothing.
#' @export
#'
#' @examples
#' library(mestrado)
#' install_iranges()
install_iranges <- function() {
  if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
  
  BiocManager::install("IRanges")
  return(invisible(NULL))
}



