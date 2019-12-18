library(magrittr)


fatia_wav <- function(wave_file) {
  wave_file <- stringr::str_replace(wave_file, "\\.wav", "")
  wav <- tuneR::readWave(glue::glue("data-raw/wav_12khz/{wave_file}.wav"))
  duracao <- length(wav@left)/wav@samp.rate
  intervalo <- 1
  n_fatias <- ceiling(duracao/intervalo - 1)
  instantes <- 0:n_fatias #round(runif(n_fatias, min = 0, max = duracao - intervalo), 1)
  fatias <- instantes %>% purrr::map(~tuneR::extractWave(wav, from = .x, to = .x+intervalo, xunit = "time"))

  purrr::walk2(fatias, instantes, ~{
    tuneR::writeWave(.x, glue::glue("data-raw/wav_12khz_1s/{wave_file}_{.y}s.wav"))
  })
}

list.files("data-raw/wav_12khz/") %>% purrr::walk(fatia_wav)



fatia_tudo <- function(wave_file) {
  wave_file <- stringr::str_replace(wave_file, "\\.wav", "")
  especie <- stringr::str_replace(wave_file, "-[0-9]+$", "")

  if(file.exists(glue::glue("data/mfcc_1s/{wave_file}_0s.rds"))) NULL

  wav <- tuneR::readWave(glue::glue("data-raw/wav_12khz/{wave_file}.wav"))
  rds <- readr::read_rds(glue::glue("data/{especie}/{wave_file}.rds"))
  duracao <- length(wav@left)/wav@samp.rate
  intervalo <- 1
  n_fatias <- ceiling(duracao/intervalo - 1)
  instantes <- 0:n_fatias #round(runif(n_fatias, min = 0, max = duracao - intervalo), 1)

  fatias <- instantes %>% purrr::map(~tuneR::extractWave(wav, from = .x, to = .x+intervalo, xunit = "time"))

  tempo <- rds$spectrograma$time
  fatias_spectro <- instantes %>% purrr::map(~{
    filtro <- tempo %>% between(.x, .x + intervalo)

    if(length(filtro) %in% ncol(rds$spectrograma$amp)) rds$spectrograma$amp[,filtro] else NULL

  })

  fatias_mfcc <- instantes %>% purrr::map(~{
    filtro <- tempo %>% between(.x, .x + intervalo)

    if(length(filtro) %in% nrow(rds$mfcc)) rds$mfcc[filtro, ] else NULL
  })

  purrr::walk2(fatias, instantes, ~{tuneR::writeWave(.x, glue::glue("data-raw/wav_12khz_1s/{wave_file}_{.y}s.wav"), extensible = FALSE)})
  purrr::walk2(fatias_spectro, instantes, ~{readr::write_rds(.x, glue::glue("data/spectro_1s/{wave_file}_{.y}s.rds"))})
  purrr::walk2(fatias_mfcc, instantes, ~{readr::write_rds(.x, glue::glue("data/mfcc_1s/{wave_file}_{.y}s.rds"))})
}

list.files("data-raw/wav_12khz/") %>% purrr::walk(fatia_tudo)



