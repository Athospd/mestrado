library(tidyverse)
library(mestrado)

# carrega segmentos dos cantos dos passarinhos
segmentations <- tidy_annotations("data_/anotacoes") %>%
  mutate(
    label = if_else(label %in% c("suggested_region", ""), str_replace(audio_id, "-[^-]+$", ""), label)
  )

# monta base de slices de 0.2 segundo
slices_200ms_labels <- retrieve_labels_for_one_dir("data-raw/wav_16khz_200ms/", segmentations, pattern = "Glaucidium|Strix-hylophila")
saveRDS(slices_200ms_labels, "data/slices_200ms_labels_by_humans.rds")
write.csv(slices_200ms_labels, "data/slices_200ms_labels_by_humans.csv", row.names = FALSE)
slices_200ms_labels %>% count(label)


# monta base de slices de 0.5 segundo
slices_500ms_labels <- retrieve_labels_for_one_dir("data-raw/wav_16khz_500ms/", segmentations, pattern = "Glaucidium|Strix-hylophila")
saveRDS(slices_500ms_labels, "data/slices_500ms_labels_by_humans.rds")
write.csv(slices_500ms_labels, "data/slices_500ms_labels_by_humans.csv", row.names = FALSE)
slices_500ms_labels %>% count(label)


# monta base de slices de 1.0 segundo
slices_1000ms_labels <- retrieve_labels_for_one_dir("data-raw/wav_16khz_1000ms/", segmentations, pattern = "Glaucidium|Strix-hylophila")
saveRDS(slices_1000ms_labels, "data/slices_1000ms_labels_by_humans.rds")
write.csv(slices_1000ms_labels, "data/slices_1000ms_labels_by_humans.csv", row.names = FALSE)
slices_1000ms_labels %>% count(label)


# monta base de slices de 2.0 segundo
slices_2000ms_labels <- retrieve_labels_for_one_dir("data-raw/wav_16khz_2000ms/", segmentations, pattern = "Glaucidium|Strix-hylophila")
saveRDS(slices_2000ms_labels, "data/slices_2000ms_labels_by_humans.rds")
write.csv(slices_2000ms_labels, "data/slices_2000ms_labels_by_humans.csv", row.names = FALSE)
slices_2000ms_labels %>% count(label)

