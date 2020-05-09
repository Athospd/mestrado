library(magrittr)
library(ggplot2)
library(patchwork)
library(warbleR)

setwd("data-raw/wav_testes/")

# "Strix-hylophila-1953532"
id = list.files("./", "Strix-hylophila") %>%
  sample(1) %>%
  stringr::str_replace(".rds", "")

##
wav_orig <- tuneR::readWave(glue::glue("{id}"))

wav_bd <- data.frame(
  sound.files = glue::glue("{id}.wav"),
  start = 0,
  end = length(wav_orig@left)/wav_orig@samp.rate
)

lspec(ovlp = 50, sxrow = 3, rows = 12, flim = c(0,10))
spec_an()
ad <- auto_detec(wl = 200, threshold = 10, ssmooth = 1000,
                 bp = c(1.2, 1.8), mindur = 0.1, flim = c(0,5),
                 maxdur = 3, img = TRUE, redo = TRUE)
