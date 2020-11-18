library(mestrado)

AUDIO_DIR = "data-raw/wavs"
AUDIO_DIR_DEST = "data-raw/BirdcallBR"
DATASET_VERSION = 1


# Slicing with 1 second long window and no overlap
AUDIO_DURATION = 1
slice_wavs(
  audio_dir = AUDIO_DIR,
  audio_dir_dest = file.path(AUDIO_DIR_DEST, paste0("birdcallbr_v", DATASET_VERSION), paste0("wavs_", AUDIO_DURATION, "000ms")),
  parallel = 4,
  interval = AUDIO_DURATION
)

# Slicing with 2 seconds long window and no overlap
AUDIO_DURATION = 2
slice_wavs(
  audio_dir = AUDIO_DIR,
  audio_dir_dest = file.path(AUDIO_DIR_DEST, paste0("birdcallbr_v", DATASET_VERSION), paste0("wavs_", AUDIO_DURATION, "000ms")),
  parallel = 4,
  interval = AUDIO_DURATION
)

# Slicing with 5 seconds long window and no overlap
AUDIO_DURATION = 5
slice_wavs(
  audio_dir = AUDIO_DIR,
  audio_dir_dest = file.path(AUDIO_DIR_DEST, paste0("birdcallbr_v", DATASET_VERSION), paste0("wavs_", AUDIO_DURATION, "000ms")),
  parallel = 6,
  interval = AUDIO_DURATION
)




