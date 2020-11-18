library(dplyr)

AUDIO_DIR = "data-raw/wavs"
AUDIO_DIR_DEST = "data-raw/BirdcallBR"
DATASET_VERSION = 1

birdcallbr_dest <- function() file.path(AUDIO_DIR_DEST, paste0("birdcallbr_v", DATASET_VERSION))


set.seed(1)
audio_ids <- tibble(audio_id = list.files(AUDIO_DIR, recursive = FALSE, pattern = "wav"))

audio_ids_train <- audio_ids %>% sample_frac(0.85)
audio_ids_test <- audio_ids %>% anti_join(audio_ids_train)

readr::write_rds(audio_ids_train, file.path(birdcallbr_dest(), "audio_ids_train.rds"))
readr::write_csv(audio_ids_train, file.path(birdcallbr_dest(), "audio_ids_train.csv"))
readr::write_rds(audio_ids_test,  file.path(birdcallbr_dest(), "audio_ids_test.rds"))
readr::write_csv(audio_ids_test,  file.path(birdcallbr_dest(), "audio_ids_test.csv"))