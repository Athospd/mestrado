library(mestrado)

# Slicing with 1 second long window and no overlap
slice_wavs(
  audio_dir = "data-raw/wavs/",
  audio_dir_dest = "data-raw/wavs_1000ms/",
  parallel = 4,
  interval = 1
)

# Slicing with 2 seconds long window and no overlap
slice_wavs(
  audio_dir = "data-raw/wavs/",
  audio_dir_dest = "data-raw/wavs_2000ms/",
  parallel = 4,
  interval = 2
)




