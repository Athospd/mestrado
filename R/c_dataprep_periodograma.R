# Redução de Resolução (downsampling)
library(magrittr)
library(doParallel)
library(foreach)

wav_16khz <- list.files("data-raw/wav_16khz/", pattern = "wav$", full.names = TRUE)
periodogramas <- stringr::str_replace(list.files("data-raw/periodogramas/", pattern = "rds$", full.names = TRUE), "rds$", "wav") %>% stringr::str_replace("periodogramas", "wav_16khz")


lista_negra <- c("data-raw/wav_16khz/Glaucidium-minutissimum-2693093.wav",
                 "data-raw/wav_16khz/Pulsatrix-koeniswaldiana-3343722.wav")
wav_16khz <- wav_16khz[!wav_16khz %in% c(periodogramas, lista_negra)]


workerFunc <- function(.x){
  wav <- tuneR::readWave(.x)

  wav@stereo <- FALSE
  wav@right <- numeric(0)
  spec <- tuneR::periodogram(wav, width = 512, overlap = 256, normalize = FALSE, frqRange = c(100, 2500), channel = "left")
  filename <- paste0("data-raw/periodogramas", stringr::str_replace(.x, "wav$", "") %>% stringr::str_extract("/[^/]*$"))
  readr::write_rds(spec, path = paste0(filename, "rds"))
  invisible(return(NULL))
}
num_cores <- detectCores()
cl <- makeCluster(4)
registerDoParallel(cl)
a<-foreach(wav = wav_16khz, .packages = c("magrittr")) %dopar% workerFunc(wav)
stopCluster(cl)

library(ggplot2)
id = list.files("data-raw/periodogramas/")[sample(1900, 1)] %>%stringr::str_replace(".rds", "")
a <- readr::read_rds(glue::glue("data-raw/periodogramas/{id}.rds"))
# b <- readr::read_rds(glue::glue("data-raw/espectrogramas/{id}..rds"))
periodograma <- a@spec %>%
  dplyr::bind_cols() %>%
  tibble::rowid_to_column("frequency") %>%
  tidyr::gather(frame, amplitude, -frequency) %>%
  dplyr::mutate(amplitude = log10(amplitude),
                frame = as.numeric(as.factor(frame)),
                frequency = - frequency) %>%

  ggplot(aes(x = frame, y = frequency, fill = amplitude)) +
  geom_raster() +
  viridis::scale_fill_viridis(option="magma") +
  theme_void()

ggspectrograma <- function(wl, ovlp, rm.noise = FALSE, s = 0.1) {
  wav <- tuneR::readWave(glue::glue("data-raw/wav_16khz/{id}.wav"))

  if(rm.noise) {
    wav@left <- smooth.spline(1:length(wav@left), wav@left, spar = s, all.knots = TRUE)$y
  }

  wav %>%
    monitoR:::spectro(wl =  wl, ovlp = ovlp) %>%
    purrr::pluck("amp") %>%
    as.data.frame() %>%
    tibble::rowid_to_column("frequency") %>%
    tidyr::gather(frame, amplitude, -frequency) %>%
    dplyr::mutate(amplitude = log10(-amplitude),
                  frame = as.numeric(as.factor(frame)),
                  frequency = frequency) %>%
    ggplot(aes(x = frame, y = frequency, fill = amplitude)) +
    geom_raster() +
    viridis::scale_fill_viridis(option="magma") +
    labs(fill = paste0("wl: ", wl, " | ovlp: ", ovlp, " noise: ", rm.noise)) +
    theme_void()
}

ggondasonora <- function(rm.noise = FALSE, s = 0.1) {
  wav <- tuneR::readWave(glue::glue("data-raw/wav_16khz/{id}.wav"))

  df <- tibble::tibble(
    time = 1:length(wav@left),
    amplitude = wav@left
  )

  if(rm.noise) {
    df <- df %>% mutate(amplitude = smooth.spline(time, amplitude, spar = s, all.knots = TRUE)$y)
  }

  df %>%
    ggplot(aes(x = time, y = amplitude)) +
    geom_line() +
    theme_void()
}

espectrograma <- ggspectrograma(512, 50)
espectrograma_n <- ggspectrograma(512, 50, TRUE, 0.3)
ondasonora <- ggondasonora()
ondasonora_n <- ggondasonora(TRUE, 0.3)

library(patchwork)

espectrograma +
  ondasonora +
  espectrograma_n +
  ondasonora_n +
  plot_layout(ncol = 2)




id = sample(list.files("data-raw/periodogramas/", "Strix-hylophila"), 1) %>%stringr::str_replace(".rds", "")

wav_orig <- tuneR::readWave(glue::glue("data-raw/wav_16khz/{id}.wav"))
wav <- tuneR::readWave(glue::glue("data-raw/wav_16khz/{id}.wav"))

wav_orig <- tuneR::readWave("Strix-hylophila-995540.wav")
wav <- tuneR::readWave("output.wav")

wav <- wav %>% seewave::bwfilter(from = 100, to = 1500, output = "Wave")
wav <- wav %>% seewave::rmnoise(spar = 0.7, output = "Wave")
wav <- wav %>% tuneR::normalize(unit = "16")




spec_orig <- wav_orig %>%
  monitoR:::spectro(wl =  512, ovlp = 50) %>%
  purrr::pluck("amp") %>%
  as.data.frame() %>%
  tibble::rowid_to_column("frequency") %>%
  tidyr::gather(frame, amplitude, -frequency) %>%
  dplyr::mutate(amplitude = -log10(-amplitude),
                frame = as.numeric(as.factor(frame)),
                frequency = frequency) %>%
  ggplot(aes(x = frame, y = frequency, fill = amplitude)) +
  geom_raster() +
  viridis::scale_fill_viridis(option="magma") +
  theme_void() +
  ggtitle(id)

spec <- wav %>%
  monitoR:::spectro(wl =  512, ovlp = 50) %>%
  purrr::pluck("amp") %>%
  as.data.frame() %>%
  tibble::rowid_to_column("frequency") %>%
  tidyr::gather(frame, amplitude, -frequency) %>%
  dplyr::mutate(
    amplitude = -log10(-amplitude),
    amplitude = if_else(amplitude %in% amplitude[1:12000], -2, amplitude),
    frame = as.numeric(as.factor(frame)),
    frequency = frequency) %>%
  ggplot(aes(x = frame, y = frequency, fill = amplitude)) +
  geom_raster() +
  viridis::scale_fill_viridis(option="magma") +
  theme_void()
id

spec_orig + spec + plot_layout(ncol = 1)


tuneR::play(wav)
tuneR::play(wav_orig)





# "Pulsatrix-koeniswaldiana-2906993"
