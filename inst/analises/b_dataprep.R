library(mestrado)

black_list <- c(
  "data-raw/mp3_originals//Glaucidium-minutissimum-2693093.mp3",
  "data-raw/mp3_originals//Pulsatrix-koeniswaldiana-3343722.mp3",
  "data-raw/mp3_originals//Pulsatrix-koeniswaldiana-3108787.mp3",
  "data-raw/mp3_originals//Megascops-choliba-3673347.mp3",
  "data-raw/mp3_originals//Megascops-choliba-3769718.mp3"
)
mp3_files <- list.files("data-raw/mp3_originals/", pattern = "mp3$", full.names = TRUE)
wavs_ok <- stringr::str_replace(list.files("data-raw/wavs", pattern = "wav$", full.names = TRUE), "wav$", "mp3") %>% stringr::str_replace("wavs", "mp3_originals/")

mp3_files <- mp3_files[!mp3_files %in% c(wavs_ok, black_list)]

wav_files <- tidy_audio(mp3_files, dest_dir = "data-raw/wavs")
