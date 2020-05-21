# Normalização
# Strix-hylophila-1881455.mp3 tem problema de amplitude muito alta.
library(magrittr)
library(doParallel)
library(foreach)

mp3 <- list.files("data-raw/mp3_originais/", pattern = "mp3$", full.names = TRUE)
wavs_normalizados <- stringr::str_replace(list.files("data-raw/wav_normalizado", pattern = "wav$", full.names = TRUE), "wav$", "mp3") %>% stringr::str_replace("wav_normalizados", "mp3_originais")


lista_negra <- c("data-raw/mp3_originais/Glaucidium-minutissimum-2693093.mp3",
                 "data-raw/mp3_originais/Pulsatrix-koeniswaldiana-3343722.mp3")
mp3 <- mp3[!mp3 %in% c(wavs_normalizados, lista_negra)]


num_cores <- detectCores()
cl <- makeCluster(4)
workerFunc <- function(.x){
  safe_readMP3 <- purrr::safely(tuneR::readMP3, otherwise = NULL)
  wav <- safe_readMP3(.x)
  if(is.null(wav$error)) {
    wav_nm <- tuneR::normalize(wav$result, unit = "16")
    filename <- paste0("data-raw/wav_normalizados/", stringr::str_replace(.x, "mp3$", "wav") %>% stringr::str_extract("/[^/]*.wav$"))
    tuneR::writeWave(wav_nm, filename = filename)
  } else {
    cat(wav$error, "\n")
    cat(wav$result, "\n")
  }
  invisible(return(NULL))
}
registerDoParallel(cl)
foreach(mp3 = mp3, .packages = c("magrittr"), .verbose = TRUE) %dopar% workerFunc(mp3)
# clusterExport(cl, varlist=c("my_data_frame"))
# parSapply(cl, mp3, workerFunc)
stopCluster(cl)




purrr::walk(mp3[30:200],
            ~{
              cat(.x, "\n")
              wav <- tuneR::readMP3(.x)
              wav_ds <- tuneR::downsample(wav, 16000)
              filename <- paste0("data-raw/wav_normalizados", stringr::str_replace(.x, "mp3$", "wav") %>% stringr::str_extract("/[^/]*.wav$"))
              cat(filename, "\n")
              tuneR::writeWave(wav_ds, filename = filename)
            }
)
