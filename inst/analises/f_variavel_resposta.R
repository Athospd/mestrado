library(tidyverse)

# carrega segmentos dos cantos dos passarinhos
segmentations <- list.files("data/anotacoes", full.names = TRUE) %>%
  purrr::map_dfr(readRDS) %>%
  drop_na() %>%
  as_tibble() %>%
  mutate(
    label = if_else(label %in% c("suggested_region", ""), str_replace(audio_id, "-[^-]+$", ""), label)
  )

retrieve_labels_for_one_dir <- function(wave_dir, segmentations, pattern = NULL) {
  slices <- tibble(slice_id = list.files(wave_dir, pattern = pattern)) %>%
    separate(slice_id, c("audio_id", "instant_start", "instant_end"), "@", remove = FALSE, extra = "drop", convert = TRUE) %>%
    mutate(audio_id = paste0(audio_id, ".wav"))

  slices_labels <- left_join(
    slices %>% group_by(audio_id) %>% nest_legacy(.key = "slices_data"),
    segmentations %>% group_by(audio_id) %>% nest_legacy(.key = "segmentations_data"),
    by = "audio_id"
  ) %>%
    mutate(
      interval_join = map2(slices_data, segmentations_data, ~{
        if(is.null(.y)) .y <- tibble(region_id = "a", start = 0, end = 1e6, label = "unknown")
        fuzzyjoin::interval_left_join(
          x = .x %>% mutate_if(is.numeric, ~.*1000),
          y = .y %>% mutate_if(is.numeric, ~.*1000),
          by = c("instant_start" = "start", "instant_end" =  "end")
        )  %>% mutate_if(is.numeric, ~./1000)
      })
    ) %>%
    select(
      audio_id,
      interval_join
    ) %>%
    unnest_legacy() %>%
    mutate(
      label = coalesce(label, "unknown")
    ) %>%
    select(
      audio_id,
      slice_id,
      label
    ) %>%
    distinct(slice_id, .keep_all = TRUE)

  return(slices_labels)
}

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

