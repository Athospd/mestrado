library(tidyverse)
library(tensorflow)
library(tfdatasets)

tamanho_dos_audios = 2
samples_per_window = 512L
stride_samples = 0.5
num_mel_bins = 64L
num_mfccs = 13L
sampling_rate = 16000L
seed = 1

set.seed(seed)
tamanho_dos_audios_em_ms <- tamanho_dos_audios * 1000L
stride_samples <- as.integer((1-stride_samples) * samples_per_window)
df <- readr::read_rds(glue::glue("data_/slices_{tamanho_dos_audios_em_ms}ms_labels_by_humans.rds")) %>%
  dplyr::mutate(
    fname = fs::as_fs_path(paste0(glue::glue("data-raw/wav_16khz_{tamanho_dos_audios_em_ms}ms/"), slice_id)),
    flag = case_when(
      label %in% "Glaucidium-minutissimum" ~ 1L,
      label %in% "Strix-hylophila" ~ 2L,
      TRUE ~ 0L
    )
  ) 
nrow_df <- nrow(df)
divisors_of_nrow <- numbers::divisors(nrow_df)
batch_size <- 1#as.integer(max(divisors_of_nrow[divisors_of_nrow <= 128]))
num_categs <- as.integer(n_distinct(df$flag))
lower_edge_hertz = 0
upper_edge_hertz = 2595 * log10(1 + (sampling_rate / 2) / 700)

range <- tf$range(
  samples_per_window %/% 2L,
  as.integer(tamanho_dos_audios*sampling_rate) - samples_per_window %/% 2L,
  stride_samples
)

n_fft_coefs <- (2 ^ tf$math$ceil(tf$math$log(tf$cast(samples_per_window, tf$float32)) / tf$math$log(2))/2 + 1L) %>% tf$cast(tf$int32)

n_periods <- tf$shape(range)[1] + 2L

# full mfcc dataset
n_coefs <- function() {browser(); a <- tf$constant(num_mfccs); a}

