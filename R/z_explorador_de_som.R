library(magrittr)
library(ggplot2)
library(tidyverse)
library(warbleR)

visualiza_segmentacao <- function(i) {
  # "Strix-hylophila-1953532"
  id = list.files("data-raw/wav_12khz/", "Pulsatrix-koeniswaldiana")[i] %>% stringr::str_replace(".wav", "")

  ##
  wav_orig <- tuneR::readWave(glue::glue("data-raw/wav_12khz/{id}.wav"))

  freq_min = 0.2
  freq_max = 0.7

  ##
  spec_orig <- wav_orig  %>%
    # seewave::bwfilter(from = freq_min*1000, to = freq_max*1000, output = "Wave") %>%
    monitoR:::spectro(wl = 512, ovlp = 50)

  ad <- warbleR::auto_detec(
    X = data.frame(sound.files = glue::glue("{id}.wav"), selec = 1, start = 0, end = Inf),
    path = "/media/athos/DATA/OneDrive/Documents/mestrado/data-raw/wav_12khz",
    img = FALSE,
    envt = "hil",
    bp = c(freq_min, freq_max),
    mindur = 0.5,
    maxdur = 2,
    ssmooth = 1100,
    threshold = 15,
    wl = 512,
    ovlp = 50,
    pb = FALSE,
    redo = TRUE
  ) %>%
    mutate_if(is.logical, replace_na, 0)


  p <- data.frame(
    amplitude = as.vector(spec_orig$amp),
    frequency = spec_orig$freq,
    time = rep(spec_orig$time, each = length(spec_orig$freq))
  ) %>%
    ggplot() +
    geom_raster(aes(x = time, y = frequency, fill = amplitude)) +
    geom_hline(data = data.frame(yintercept = c(freq_min, freq_max)), aes(yintercept = yintercept)) +
    geom_rect(data = ad, aes(xmin = start, xmax = end, ymin = -1 + runif(length(start), 0, 1), ymax = 5  + runif(length(start), -1, 1)), alpha = 0.0, colour = "black", size = 0.5) +
    viridis::scale_fill_viridis(option="magma") +
    theme_minimal() +
    ggtitle(id) +
    theme(legend.position = "bottom")
  print(p)

  return(id)
}
i=161
id = visualiza_segmentacao(i);
i = i + 1

id = id
tuneR::play(glue::glue("data-raw/wav_12khz/{id}.wav"))

teste <- readWave(glue::glue("data-raw/wav_12khz/{id}.wav")) %>%
  tuneR::mono(which = "both") %>%
  seewave::bwfilter(from = 1200, to = 1600, bandpass = TRUE, output = "Wave", listen = TRUE)
tuneR::play(teste)
