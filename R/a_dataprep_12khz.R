
#########################################
library(magrittr)
library(doMC)
library(foreach)


mp3 <- list.files("data-raw/mp3_originais/", pattern = "mp3$", full.names = TRUE)
wavs_ok <- stringr::str_replace(list.files("data-raw/wav_12khz/", pattern = "wav$", full.names = TRUE), "wav$", "mp3") %>% stringr::str_replace("wav_12khz", "mp3_originais")


lista_negra <- c("data-raw/mp3_originais//Glaucidium-minutissimum-2693093.mp3",
                 "data-raw/mp3_originais//Pulsatrix-koeniswaldiana-3343722.mp3",
                 "data-raw/mp3_originais//Pulsatrix-koeniswaldiana-3108787.mp3")
mp3 <- mp3[!mp3 %in% c(wavs_ok, lista_negra)]


workerFunc <- function(.x){
  wav <- tuneR::readMP3(.x)
  wav@left <- as.numeric(wav@left) %>% tidyr::replace_na(0)
  wav@right <- as.numeric(wav@right) %>% tidyr::replace_na(0)

  if(wav@stereo) wav <- wav %>% tuneR::mono(which = "both")

  wav@left <- as.integer(wav@left)
  wav@right <- as.integer(wav@right)

  wav <- wav %>%
    tuneR::normalize(unit = "16") %>%
    tuneR::downsample(12000)

  filename <- paste0("data-raw/wav_12khz/", stringr::str_replace(.x, "mp3$", "wav") %>% stringr::str_extract("/[^/]*.wav$"))
  tuneR::writeWave(wav, filename = filename, extensible = FALSE)

  invisible(return(NULL))
}

num_cores <- detectCores()
registerDoMC(8)
foreach(mp3 = mp3, .packages = c("magrittr", "tuneR", "stringr")) %do% workerFunc(mp3)


purrr::walk(mp3, workerFunc)
