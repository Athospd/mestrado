library(magrittr)
library(furrr)

slice_one_wav_randomly <- function() {}

slice_one_wav_sequencially <- function(wave_file, wave_dir, wave_dir_dest, interval = 1, overlap = 0) {
  wave_file <- stringr::str_replace(wave_file, "\\.wav", "")
  wav <- tuneR::readWave(glue::glue("{wave_dir}/{wave_file}.wav"))
  duration <- length(wav@left)/wav@samp.rate
  instants_ini <- seq(0, duration, by = interval - overlap)
  slices <- purrr::map(instants_ini, ~tuneR::extractWave(wav, from = .x, to = .x+interval, xunit = "time"))

  purrr::walk2(slices, instants_ini, ~{
    instants_end = .y + interval
    instants_end = ifelse(instants_end > duration, round(duration, 4), instants_end)
    tuneR::writeWave(.x, glue::glue("{wave_dir_dest}/{wave_file}@{.y}@{instants_end}@.wav"))
  })
  return(NULL)
}

slice_wavs <- function(wave_dir, wave_dir_dest = NULL, interval = 1, parallel = FALSE, workers = 7) {

  wave_dir = stringr::str_replace(wave_dir, "/$", "")

  if(parallel) {
    plan(multiprocess, workers = workers)
  }

  if(is.null(wave_dir_dest)) {
    wave_dir_dest = glue::glue("{wave_dir}_{interval*1000}ms")
  }

  if(!fs::dir_exists(wave_dir_dest)) {
    fs::dir_create(wave_dir_dest)
  }

  list.files(wave_dir, pattern = "wav$") %>%
    furrr::future_map(
      slice_one_wav_sequencially,
      wave_dir = wave_dir,
      wave_dir_dest = wave_dir_dest,
      interval = interval,
      .progress = TRUE
    )
}



c(1, 2) %>% purrr::map(~ slice_wavs(
  wave_dir = "data-raw/wav_16khz/",
  parallel = TRUE,
  interval = .x,
  workers = 7
))



