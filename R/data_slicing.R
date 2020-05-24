
#' slice_one_wav_randomly
#' 
#' To do.
#'
#' @return
slice_one_wav_randomly <- function() {
  usethis::ui_oops("Not implemented yet.")
}

#' Slice one Wave File Sequentially
#' 
#' Slice one wave file sequentially by regular interval of time with ou without overlap.
#'
#' @param audio_file 
#' @param audio_dir 
#' @param audio_dir_dest 
#' @param interval 
#' @param overlap 
#' @param invisible
#'
#' @return one character with the directory where the slices were stored. 
slice_one_wav_sequencially <- function(audio_file, audio_dir, audio_dir_dest, interval = 1, overlap = 0, invisible = FALSE) {
  audio_file_ext <- tools::file_ext(audio_file)
  if(!audio_file_ext %in% c("mp3", "wav")) {
    stop("Audio file must be either .mp3 or .wav. Got ", audio_file_ext, ".")
  }
  
  if(!fs::dir_exists(audio_dir_dest)) {
    fs::dir_create(audio_dir_dest)
  }
  
  audio_file_name <- tools::file_path_sans_ext(audio_file)
  audio_file_path <- glue::glue("{audio_dir}/{audio_file}")
  if(audio_file_ext == "mp3") {
    wav <- tuneR::readMP3(audio_file_path)
  } else {
    wav <- tuneR::readWave(audio_file_path)
  }
  
  if(wav@stereo) {
    wav <- tuneR::mono(wav, "left")
  }
  
  duration <- length(wav@left)/wav@samp.rate
  instants_ini <- seq(0, duration, by = interval - overlap)
  slices <- purrr::map(instants_ini, ~tuneR::extractWave(wav, from = .x, to = .x+interval, xunit = "time"))
  
  purrr::walk2(slices, instants_ini, ~{
    instants_end = .y + interval
    instants_end = ifelse(instants_end > duration, round(duration, 4), instants_end)
    tuneR::writeWave(.x, glue::glue("{audio_dir_dest}/{audio_file_name}@{.y}@{instants_end}@.wav"))
  })
  
  if(invisible) {
    return(invisible(NULL))
  } else {
    return(audio_dir_dest)
  }
}

#' Slice a Set of Wave Files
#' 
#' Vectorized version of slice_one_wav_*() family of functions. Store in disk 
#' the slices of every file of a vector of mp3 file names.
#'
#' @param audio_dir 
#' @param audio_dir_dest 
#' @param audio_ext 
#' @param interval 
#' @param parallel 
#'
#' @return
#' @export
#'
#' @examples
#' 
#' library(mestrado)
#' 
#' wav_dir <- system.file("wav_sample", package = "mestrado")
#' temp_dir <- tempdir()
#' 
#' slices_path <- slice_wavs(wav_dir, temp_dir)
#' slices_path
#' 
#' slices <- list.files(slices_path)
#' slices[4:7]
slice_wavs <- function(audio_dir, audio_dir_dest = NULL, interval = 1, parallel = 1) {
  
  audio_dir = stringr::str_replace(audio_dir, "/$", "")
  
  if(parallel > 1) {
    requireNamespace("future", quietly = TRUE)
    future::plan(future::multiprocess, workers = parallel)
  }
  
  if(is.null(audio_dir_dest)) {
    audio_dir_dest = glue::glue("{audio_dir}_{interval*1000}ms")
  }
  
  if(!fs::dir_exists(audio_dir_dest)) {
    fs::dir_create(audio_dir_dest)
  }
  
  invisible(
    list.files(audio_dir, pattern = paste0("wav", "$")) %>%
    furrr::future_map_chr(
      slice_one_wav_sequencially,
      audio_dir = audio_dir,
      audio_dir_dest = audio_dir_dest,
      interval = interval,
      .progress = TRUE,
      invisible = FALSE
    ) %>%
      unique()
  )
}