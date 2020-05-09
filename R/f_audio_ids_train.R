library(dplyr)

set.seed(1)
audio_ids_train <- readr::read_rds(glue::glue("data/slices_1000ms_labels_by_humans.rds")) %>% 
  distinct(audio_id) %>% 
  sample_frac(0.85)

readr::write_rds(audio_ids_train, "data/audio_ids_train.rds")
