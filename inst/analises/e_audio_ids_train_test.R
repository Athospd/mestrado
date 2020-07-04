library(dplyr)

set.seed(1)
audio_ids <- readr::read_rds(glue::glue("data_/slices_1000ms_labels_by_humans.rds")) %>% distinct(audio_id)

audio_ids_train <- audio_ids %>% sample_frac(0.85)
audio_ids_test <- audio_ids %>% anti_join(audio_ids_train)

readr::write_rds(audio_ids_train, "data_/audio_ids_train.rds")
readr::write_rds(audio_ids_test, "data_/audio_ids_test.rds")
