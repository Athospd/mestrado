# ESTE ARQUIVO DEMORA MUITO

AUDIO_DIR_DEST = "data-raw/BirdcallBR"
DATASET_VERSION = 1

birdcallbr_dest <- function() file.path(AUDIO_DIR_DEST, paste0("birdcallbr_v", DATASET_VERSION))


AUDIO_DURATION = 1
old_wd <- getwd()
setwd(birdcallbr_dest())
waves <- readRDS(paste0("wavs_", AUDIO_DURATION, "000ms_labels_by_humans.rds"))
zip(zipfile = paste0("birdcallbr_v", DATASET_VERSION, "_", AUDIO_DURATION,"000ms.zip") , files = c(
  file.path(paste0("wavs_", AUDIO_DURATION, "000ms"), waves$slice_id), 
  paste0("wavs_", AUDIO_DURATION, "000ms_labels_by_humans.csv"),
  paste0("wavs_", AUDIO_DURATION, "000ms_labels_by_humans.rds"),
  "audio_ids_train.rds",
  "audio_ids_train.csv",
  "audio_ids_test.rds",
  "audio_ids_test.csv"
))
setwd(old_wd)


AUDIO_DURATION = 2
old_wd <- getwd()
setwd(birdcallbr_dest())
zip(zipfile = paste0("birdcallbr_v", DATASET_VERSION, "_", AUDIO_DURATION,"000ms.zip") , files = c(
  list.files(paste0("wavs_", AUDIO_DURATION, "000ms"), full.names = TRUE)[1:4], 
  paste0("wavs_", AUDIO_DURATION, "000ms_labels_by_humans.csv"),
  paste0("wavs_", AUDIO_DURATION, "000ms_labels_by_humans.rds"),
  "audio_ids_train.rds",
  "audio_ids_train.csv",
  "audio_ids_test.rds",
  "audio_ids_test.csv"
))
setwd(old_wd)


AUDIO_DURATION = 5
old_wd <- getwd()
setwd(birdcallbr_dest())
zip(zipfile = paste0("birdcallbr_v", DATASET_VERSION, "_", AUDIO_DURATION,"000ms.zip") , files = c(
  list.files(paste0("wavs_", AUDIO_DURATION, "000ms"), full.names = TRUE)[1:4], 
  paste0("wavs_", AUDIO_DURATION, "000ms_labels_by_humans.csv"),
  paste0("wavs_", AUDIO_DURATION, "000ms_labels_by_humans.rds"),
  "audio_ids_train.rds",
  "audio_ids_train.csv",
  "audio_ids_test.rds",
  "audio_ids_test.csv"
))
setwd(old_wd)
