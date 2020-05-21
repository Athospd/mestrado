# Redução de Resolução (downsampling)
library(magrittr)
library(doParallel)
library(foreach)

wav_declipados <- list.files("data-raw/wav_declipados/", pattern = "wav$", full.names = TRUE)
wav_16khz <- stringr::str_replace(list.files("data-raw/wav_16khz/", pattern = "wav$", full.names = TRUE), "wav$", "wav") %>% stringr::str_replace("wav_16khz", "wav_declipados")


lista_negra <- c("data-raw/wav_declipados/Glaucidium-minutissimum-2693093.wav",
                 "data-raw/wav_declipados/Pulsatrix-koeniswaldiana-3343722.wav")
wav_declipados <- wav_declipados[!wav_declipados %in% c(wav_16khz, lista_negra)]


workerFunc <- function(.x){
  wav <- tuneR::readWave(.x)
  wav_ds <- tuneR::downsample(wav, 16000)
  filename <- paste0("data-raw/wav_16khz", stringr::str_replace(.x, "wav$", "wav") %>% stringr::str_extract("/[^/]*.wav$"))
  tuneR::writeWave(wav_ds, filename = filename)
  invisible(return(NULL))
}
num_cores <- detectCores()
cl <- makeCluster(8)
registerDoParallel(cl)
a<-foreach(wav = wav_declipados, .packages = c("magrittr")) %dopar% workerFunc(wav)
stopCluster(cl)




purrr::walk(wav_16khz[1], workerFunc)
