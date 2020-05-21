# Deteccao de "clipping"
clip_detect.character <- function(x, threshold = 99) {

  file_ext <- tools::file_ext(x)

  if(file_ext == "mp3") {
    x <- tuneR::readMP3(x)
  } else if (file_ext == "wav") {
    x <- tuneR::readWave(x)
  } else {
    stop("File must be mp3 or wav format.")
  }

  clip_detect.Wave(x)
}

clip_detect.Wave <- function(x, threshold = 0.99) {

  clip_on_left <- clip_detect.default(x@left, x@bit, threshold)
  clip_on_right <- FALSE

  if(x@stereo) {
    clip_on_right <- clip_detect.default(x@right, x@bit, threshold)
  }

  return(clip_on_left | clip_on_right)
}

clip_detect.default <- function(x, bit, threshold = 0.99) {
  any(abs(x) > ((2^(bit - 1)) * threshold))
}

clip_detect <- function(object, ...) {
  UseMethod("clip_detect")
}

####
clip_remove.character <- function(x, threshold = 99) {

  file_ext <- tools::file_ext(x)

  if(file_ext == "mp3") {
    x <- tuneR::readMP3(x)
  } else if (file_ext == "wav") {
    x <- tuneR::readWave(x)
  } else {
    stop("File must be mp3 or wav format.")
  }

  clip_remove.Wave(x)
}

clip_remove.Wave <- function(x, threshold = 0.99) {

  clip_on_left <- abs(x@left) > ((2^(x@bit - 1)) * threshold)
  clip_on_right <- logical(length(x@left))

  if(x@stereo) {
    clip_on_right <- abs(x@right) > ((2^(x@bit - 1)) * threshold)
  }

  x@left <- x@left[!clip_on_left & !clip_on_right]

  if(x@stereo) {
    x@right <- x@right[!clip_on_left & !clip_on_right]
  }

  return(x)
}

clip_remove.default <- function(x, bit, threshold = 0.99) {
  x[abs(x) < ((2^(bit - 1)) * threshold)]
}

clip_remove <- function(object, ...) {
  UseMethod("clip_remove")
}

#########################################
library(magrittr)
library(doParallel)
library(foreach)


mp3 <- list.files("data-raw/mp3_originais/", pattern = "mp3$", full.names = TRUE)
wavs_declipados <- stringr::str_replace(list.files("data-raw/wav_declipados", pattern = "wav$", full.names = TRUE), "wav$", "mp3") %>% stringr::str_replace("wav_declipados", "mp3_originais")


lista_negra <- c("data-raw/mp3_originais/Glaucidium-minutissimum-2693093.mp3",
                 "data-raw/mp3_originais/Pulsatrix-koeniswaldiana-3343722.mp3",
                 "data-raw/mp3_originais/Pulsatrix-koeniswaldiana-3108787.mp3")
mp3 <- mp3[!mp3 %in% c(wavs_declipados, lista_negra)]


workerFunc <- function(.x){
  wav <- tuneR::readMP3(.x)
  wav_cp <- clip_remove(wav, threshold = 0.9)
  filename <- paste0("data-raw/wav_declipados/", stringr::str_replace(.x, "mp3$", "wav") %>% stringr::str_extract("/[^/]*.wav$"))
  tuneR::writeWave(wav_cp, filename = filename)

  invisible(return(NULL))
}

num_cores <- detectCores()
cl <- makeCluster(4)
registerDoParallel(cl)
foreach(mp3 = mp3, .packages = c("magrittr"), .export = c("clip_remove", "clip_remove.default", "clip_remove.Wave", "clip_remove.character")) %dopar% workerFunc(mp3)
stopCluster(cl)


purrr::walk(mp3, workerFunc)
