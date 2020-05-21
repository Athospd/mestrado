library(tidyverse)
library(tensorflow)
tfe_enable_eager_execution(device_policy = "silent")

fname <- list.files("data-raw/wav_12khz/", full.names = TRUE) %>% sample(1)
wav_aff <- tuneR::readWave(fname)

tuneR::writeWave(wav_aff, "teste.wav", extensible = FALSE)

file <- tf$read_file(fname)

wav <- tf$audio$decode_wav(file)
sampling_rate <- wav$sample_rate %>% as.numeric()
sampling_rate

samples <- wav$audio
samples <- samples %>% tf$transpose(perm = c(1L, 0L))
samples

window_size_ms <- 1000*512/sampling_rate
window_stride_ms <- window_size_ms/2

samples_per_window <- sampling_rate * window_size_ms/1000
stride_samples <-  sampling_rate * window_stride_ms/1000

stft_out <- tf$signal$stft(
  samples,
  frame_length = as.integer(samples_per_window),
  frame_step = as.integer(stride_samples)
)

magnitude_spectrograms <- tf$abs(stft_out)


lower_edge_hertz <- 0
upper_edge_hertz <- 2595 * log10(1 + (sampling_rate/2)/700)
num_mel_bins <- 64L
num_spectrogram_bins <- magnitude_spectrograms$shape[-1]$value

linear_to_mel_weight_matrix <- tf$signal$linear_to_mel_weight_matrix(
  num_mel_bins,
  num_spectrogram_bins,
  sampling_rate,
  lower_edge_hertz,
  upper_edge_hertz
)

mel_spectrograms <- tf$tensordot(magnitude_spectrograms, linear_to_mel_weight_matrix, 1L)
log_mel_spectrograms <- tf$log(mel_spectrograms + 1e-6)

num_mfccs <- 13
mfccs <- tf$signal$mfccs_from_log_mel_spectrograms(log_mel_spectrograms)[, , 1:num_mfccs]


