library(tidyverse)
library(tensorflow)
tfe_enable_eager_execution(device_policy = "silent")

fname <- list.files("data-raw/wav_12khz/", full.names = TRUE) %>% sample(1)

data_generator <- function(df,
                           samples_per_window,
                           stride_samples) {

  # assume sampling rate is the same in all samples
  sampling_rate <-
    tf$audio$decode_wav(tf$read_file(tf$reshape(df$fname[[1]], list()))) %>% .$sample_rate

  # samples_per_window <- (sampling_rate * window_size_ms) %/% 1000L
  # stride_samples <-  (sampling_rate * window_stride_ms) %/% 1000L

  n_periods <-
    tf$shape(
      tf$range(
        samples_per_window %/% 2L,
        as.integer(sampling_rate) - samples_per_window %/% 2L,
        stride_samples
      )
    )[1] + 1L

  n_fft_coefs <-
    (2 ^ tf$ceil(tf$log(
      tf$cast(samples_per_window, tf$float32)
    ) / tf$log(2)) /
      2 + 1L) %>% tf$cast(tf$int32)

  ds <- tensor_slices_dataset(df) %>%
    dataset_shuffle(buffer_size = buffer_size)

  ds <- ds %>%
    dataset_map(function(obs) {
      wav <-
        tf$audio$decode_wav(tf$read_file(tf$reshape(obs$fname, list())))
      samples <- wav$audio
      samples <- samples %>% tf$transpose(perm = c(1L, 0L))

      stft_out <- tf$signal$stft(samples,
                                 frame_length = samples_per_window,
                                 frame_step = stride_samples)

      magnitude_spectrograms <- tf$abs(stft_out)
      log_magnitude_spectrograms <- tf$log(magnitude_spectrograms + 1e-6)

      response <- tf$one_hot(obs$class_id, 30L)

      input <- tf$transpose(log_magnitude_spectrograms, perm = c(1L, 2L, 0L))
      list(input, response)
    })

  ds <- ds %>%
    dataset_repeat()

  ds %>%
    dataset_padded_batch(
      batch_size = batch_size,
      padded_shapes = list(tf$stack(list(
        n_periods, n_fft_coefs,-1L
      )),
      tf$constant(-1L, shape = shape(1L))),
      drop_remainder = TRUE
    )
}
