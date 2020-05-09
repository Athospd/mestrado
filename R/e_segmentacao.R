library(magrittr)
library(tidyverse)
library(furrr)
library(magrittr)
library(ggplot2)
library(warbleR)
plan(multisession)

gera_segmentacao_de_um_id <- function(id, params_do_auto_detec, path = "data-raw/wav_12khz/") {
  ## carrega wav
  wav_name <- glue::glue("data-raw/wav_12khz/{id}.wav")
  if(!file.exists(wav_name)) return(NULL)
  wav <- tuneR::readWave(wav_name)

  ## spectrograma
  spec <- wav %>% monitoR:::spectro(wl =  512, ovlp = 50)

  ## mfcc
  mfcc <- wav %>% tuneR:::melfcc(wintime =  512/wav@samp.rate, hoptime = 512/2/wav@samp.rate)

  ## funcao do auto detector
  auto_detect_partial <- purrr::partial(
    warbleR::auto_detec,
    X = data.frame(sound.files = glue::glue("{id}.wav"), selec = 1, start = 0, end = Inf),
    path = path,
    pb = FALSE
  )

  ## segmentacoes encontradas
  ad <- do.call(auto_detect_partial, params_do_auto_detec)

  ## grafico
  grafico <- data.frame(
    amplitude = as.vector(spec$amp),
    frequency = spec$freq,
    time = rep(spec$time, each = length(spec$freq))
  ) %>%
    ggplot() +
    geom_raster(aes(x = time, y = frequency, fill = amplitude)) +
    geom_hline(data = data.frame(yintercept = c(params_do_auto_detec$bp[1], params_do_auto_detec$bp[2])), aes(yintercept = yintercept)) +
    geom_rect(data = ad, aes(xmin = start, xmax = end, ymin = -2 + runif(length(start), 0, 1), ymax = 7  + runif(length(start), -1, 1)), alpha = 0.0, colour = "black", size = 0.5) +
    viridis::scale_fill_viridis(option="magma") +
    theme_minimal() +
    ggtitle(id) +
    theme(legend.position = "bottom")

  return(
    list(
      id = id,
      spectrograma = spec,
      mfcc = mfcc,
      segmentacao = ad,
      grafico = grafico
    )
  )
}

gera_segmentacao_de_uma_especie <- function(especie, params_do_auto_detec, path = "data-raw/wav_12khz/") {
  ids = list.files(path, especie) %>% stringr::str_replace("\\..*$", "")
  ids_que_ja_foram = list.files(glue::glue("data/{especie}")) %>% stringr::str_replace("\\..*$", "")

  ids = ids[!ids %in% ids_que_ja_foram]
  pb <- progress::progress_bar$new(total = length(ids))


  segmentacoes <- furrr::future_map(ids, ~{
    pb$tick()
    lista <- gera_segmentacao_de_um_id(.x, params_do_auto_detec, path)
    readr::write_rds(lista, glue::glue("data/{especie}/{.x}.rds"))
    return(NULL)
  })

  return(segmentacoes)
}

passaros <- tibble::tibble(
  especie = c("Megascops-choliba",
              "Strix-hylophila",
              "Pulsatrix-koeniswaldiana",
              "Megascops-atricapilla",
              "Glaucidium-minutissimum")
) %>%
  mutate(
    parametros_do_auto_detect = list(
      list(
        img = FALSE,
        envt = "hil",
        bp = c(0.55, 1.05),
        mindur = 0.5,
        maxdur = 2,
        ssmooth = 1200,
        threshold = 15,
        wl = 512,
        ovlp = 50
      ),

      list(
        img = FALSE,
        envt = "hil",
        bp = c(0.2, 1.0),
        mindur = 0.5,
        maxdur = 6,
        ssmooth = 1600,
        threshold = 15,
        wl = 512,
        ovlp = 50
      ),

      list(
        wl = 200, threshold = 10, ssmooth = 1000,
        bp = c(1.2, 1.8), mindur = 0.1, flim = c(0,5),
        maxdur = 3, img = TRUE, redo = TRUE
      ),

      list(
        wl = 200, threshold = 10, ssmooth = 1000,
        bp = c(1.2, 1.8), mindur = 0.1, flim = c(0,5),
        maxdur = 3, img = TRUE, redo = TRUE
      ),

      list(
        img = FALSE,
        envt = "hil",
        bp = c(1.2, 1.55),
        mindur = 0.2,
        maxdur = 0.8,
        ssmooth = 1500,
        threshold = 15,
        wl = 512,
        ovlp = 50
      )
    ),
    parametros_do_auto_detect = parametros_do_auto_detect %>% setNames(especie)
  )


especie <- passaros$especie[5]
params_do_auto_detec <- passaros$parametros_do_auto_detect[[5]]
dir <- glue::glue("data/{especie}")
if(!dir.exists(dir)) dir.create(dir)
segmentacoes <- gera_segmentacao_de_uma_especie(especie, params_do_auto_detec)

