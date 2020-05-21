library(magrittr)
library(furrr)

slice_one_wav_randomly <- function() {}

slice_one_wav_sequencially <- function(audio_file, audio_dir, audio_dir_dest, interval = 1, overlap = 0) {
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
    wav <- tuneR::mono(wav, "both")
  }
  
  duration <- length(wav@left)/wav@samp.rate
  instants_ini <- seq(0, duration, by = interval - overlap)
  slices <- purrr::map(instants_ini, ~tuneR::extractWave(wav, from = .x, to = .x+interval, xunit = "time"))

  purrr::walk2(slices, instants_ini, ~{
    instants_end = .y + interval
    instants_end = ifelse(instants_end > duration, round(duration, 4), instants_end)
    tuneR::writeWave(.x, glue::glue("{audio_dir_dest}/{audio_file_name}@{.y}@{instants_end}@.wav"))
  })
  return(audio_dir_dest)
}

slice_wavs <- function(audio_dir, audio_dir_dest = NULL, audio_ext = "(wav|mp3)", interval = 1, parallel = FALSE, workers = 7) {

  audio_dir = stringr::str_replace(audio_dir, "/$", "")

  if(parallel) {
    plan(multiprocess, workers = workers)
  }

  if(is.null(audio_dir_dest)) {
    audio_dir_dest = glue::glue("{audio_dir}_{interval*1000}ms")
  }

  if(!fs::dir_exists(audio_dir_dest)) {
    fs::dir_create(audio_dir_dest)
  }

  list.files(audio_dir, pattern = paste0(audio_ext, "$")) %>%
    furrr::future_map(
      slice_one_wav_sequencially,
      audio_dir = audio_dir,
      audio_dir_dest = audio_dir_dest,
      interval = interval,
      .progress = TRUE
    )
}



c(1) %>% purrr::map(~ slice_wavs(
  audio_dir = "data-raw/mp3_originals/",
  parallel = TRUE,
  interval = .x,
  workers = 7
))




