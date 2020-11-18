library(tidyverse)
library(mestrado)

ANNOTATIONS_DIR = "data-raw/annotations"
AUDIO_DIR_DEST = "data-raw/BirdcallBR"
DATASET_VERSION = 1
PATTERN = "Glaucidium|Strix-hylophila"

audio_dir_dest <- function() file.path(AUDIO_DIR_DEST, paste0("birdcallbr_v", DATASET_VERSION), paste0("wavs_", AUDIO_DURATION, "000ms"))
audio_labels_dest <- function(extension) file.path(AUDIO_DIR_DEST, paste0("birdcallbr_v", DATASET_VERSION), paste0("wavs_", AUDIO_DURATION, "000ms_labels_by_humans", extension))

# carrega segmentos dos cantos dos passarinhos
annotations <- tidy_annotations(ANNOTATIONS_DIR) %>%
  mutate(
    label = if_else(label %in% c("suggested_region", ""), stringr::str_replace(audio_id, "-[^-]+$", ""), label)
  )

# monta base de slices de 1.0 segundo
AUDIO_DURATION = 1
wavs_labels <- label_slices(audio_dir_dest(), annotations, pattern = PATTERN)
saveRDS(wavs_labels, audio_labels_dest(".rds"))
write.csv(wavs_labels, audio_labels_dest(".csv"), row.names = FALSE)
wavs_labels %>% count(label)


# monta base de slices de 2.0 segundos
AUDIO_DURATION = 2
wavs_labels <- label_slices(audio_dir_dest(), annotations, pattern = PATTERN)
saveRDS(wavs_labels, audio_labels_dest(".rds"))
write.csv(wavs_labels, audio_labels_dest(".csv"), row.names = FALSE)
wavs_labels %>% count(label)


# monta base de slices de 5.0 segundos
AUDIO_DURATION = 5
wavs_labels <- label_slices(audio_dir_dest(), annotations, pattern = PATTERN)
saveRDS(wavs_labels, audio_labels_dest(".rds"))
write.csv(wavs_labels, audio_labels_dest(".csv"), row.names = FALSE)
wavs_labels %>% count(label)
