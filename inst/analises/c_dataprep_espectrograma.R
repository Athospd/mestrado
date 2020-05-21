# Redução de Resolução (downsampling)
library(magrittr)
library(doParallel)
library(foreach)

wav_16khz <- list.files("data-raw/wav_16khz/", pattern = "wav$", full.names = TRUE)
espectrogramas <- stringr::str_replace(list.files("data-raw/espectrogramas/", pattern = "wav$", full.names = TRUE), "wav$", "wav") %>% stringr::str_replace("espectrogramas", "wav_16khz")


lista_negra <- c("data-raw/wav_16khz/Glaucidium-minutissimum-2693093.wav",
                 "data-raw/wav_16khz/Pulsatrix-koeniswaldiana-3343722.wav")
wav_16khz <- wav_16khz[!wav_16khz %in% c(espectrogramas, lista_negra)]


num_cores <- detectCores()
cl <- makeCluster(8)
workerFunc <- function(.x){
  wav <- tuneR::readWave(.x)
  spec <- monitoR:::spectro(wav, wl =  512, ovlp = 50)
  filename <- paste0("data-raw/espectrogramas", stringr::str_replace(.x, "wav$", "") %>% stringr::str_extract("/[^/]*$"))
  readr::write_rds(spec, path = paste0(filename, ".rds"))
  invisible(return(NULL))
}
registerDoParallel(cl)
a<-foreach(wav = wav_16khz, .packages = c("magrittr")) %dopar% workerFunc(wav)
stopCluster(cl)




a<- "data-raw/wav_16khz/Glaucidium-minutissimum-1066225.wav" %>% tuneR::readWave()
tuneR::powspec(a@left,16000) %>%
  as.data.frame %>%
  tibble::rowid_to_column("frequency") %>%
  tidyr::gather(frame, amplitute, -frequency) %>%
  ggplot(aes(x = frame, y = frequency, fill = amplitute)) +
  geom_raster() +
  viridis::scale_fill_viridis(option="magma")
