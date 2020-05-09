# This code should run with TensorFlow >= 1.13.
# It was designed to run in graph as well as eager modes.
# If running in eager mode only, some pieces of code can be substantially simplified.

library(tidyverse)
library(tensorflow)

library(tfdatasets)

library(stringr)
library(dplyr)

use_session_with_seed(7777,
                      disable_gpu = TRUE,
                      disable_parallel_cpu = TRUE)


df <- readr::read_rds("data/slices_1000ms_labels_by_humans.rds") %>%
  mutate(
    fname = fs::as_fs_path(paste0("data-raw/wav_16khz_1000ms/", slice_id)),
    flag = case_when(
      label %in% "Glaucidium-minutissimum" ~ 1L,
      label %in% "Strix-hylophila" ~ 2L,
      TRUE ~ 0L
    )
  )


batch_size <- 32
buffer_size <- nrow(df)

type = "log_magnitude"

samples_per_window <- 512L
stride_samples <-  as.integer(samples_per_window/2)
lower_edge_hertz <- 0
upper_edge_hertz <- 2595 * log10(1 + (16000 / 2) / 700)
num_mel_bins <- 64L
num_mfccs <- 13L

data_generator <- function(df,
                           samples_per_window,
                           stride_samples,
                           sampling_rate = 16000) {
  # samples_per_window <- (sampling_rate * window_size_ms) %/% 1000L   # unit = samples
  # stride_samples <-  (sampling_rate * window_stride_ms) %/% 1000L   # unit = samples

  n_periods <-
    tf$shape(
      tf$range(
        samples_per_window %/% 2L,
        16000L - samples_per_window %/% 2L,
        stride_samples
      )
    )[1] + 1L

  n_fft_coefs <-
    (2 ^ tf$math$ceil(tf$math$log(
      tf$cast(samples_per_window, tf$float32)
    ) / tf$math$log(2)) /
      2 + 1L) %>% tf$cast(tf$int32)

  ds <- tensor_slices_dataset(df) %>%
    dataset_shuffle(buffer_size = buffer_size)

  ds <- ds %>%
    dataset_map(function(obs) {
      wav <- tf$audio$decode_wav(tf$io$read_file(tf$reshape(obs$fname, list())), desired_channels = 1L)
      samples <- wav$audio
      samples <- samples %>% tf$transpose(perm = c(1L, 0L))

      stft_out <- tf$signal$stft(samples,
                                 frame_length = as.integer(samples_per_window),
                                 frame_step = as.integer(stride_samples))

      magnitude_spectrograms <- tf$abs(stft_out)

      if (type == "log_magnitude") {
        log_magnitude_spectrograms <- tf$math$log(magnitude_spectrograms + 1e-6)

      } else if (type == "log_mel" || type == "mfcc") {
        # can't use when building static graph as it needs evaluation
        # num_spectrogram_bins <- magnitude_spectrograms$shape[-1]$value
        linear_to_mel_weight_matrix <-
          tf$signal$linear_to_mel_weight_matrix(
            num_mel_bins,
            num_spectrogram_bins=257,
            sample_rate=16000,
            lower_edge_hertz=lower_edge_hertz,
            upper_edge_hertz=upper_edge_hertz
          )
        mel_spectrograms <- tf$tensordot(magnitude_spectrograms,
                                         linear_to_mel_weight_matrix,
                                         1L)
        log_mel_spectrograms <- tf$math$log(mel_spectrograms + 1e-6)

        if (type == "mfcc") {
          # Keep the first `num_mfccs` MFCCs.
          mfccs <- tf$signal$mfccs_from_log_mel_spectrograms(log_mel_spectrograms)[ , , 1:num_mfccs]
        }

      }

      response <- tf$one_hot(obs$flag, 3L)

      if (type == "log_magnitude") {
        # move batch dimension to channels
        input <-
          tf$transpose(log_magnitude_spectrograms, perm = c(1L, 2L, 0L))

      } else if (type == "log_mel") {
        input <-
          tf$transpose(log_mel_spectrograms, perm = c(1L, 2L, 0L))

      } else if (type == "mfcc") {
        input <-
          tf$transpose(mfccs, perm = c(1L, 2L, 0L))
      }

      list(input, response)

    })

  n_coefs <-  if (type == "log_magnitude") {
    n_fft_coefs
  } else if (type == "log_mel") {
    tf$constant(num_mel_bins)
  } else if (type == "mfcc") {
    tf$constant(num_mfccs)
  }
  my_dataset_padded_batch <- function (dataset, batch_size, padded_shapes, padding_values = NULL, drop_remainder = FALSE) {
      as_tf_dataset(dataset$padded_batch(batch_size = tfdatasets:::as_integer_tensor(batch_size),
                                         padded_shapes = tfdatasets:::as_tensor_shapes(padded_shapes),
                                         padding_values = padding_values,
                                         drop_remainder = TRUE))
  }

  ds <- ds %>%
    dataset_repeat()
  ds %>%
    my_dataset_padded_batch(
      batch_size = batch_size,
      padded_shapes = list(tf$stack(list(
        n_periods+1L, n_coefs, -1L
      )),
      tf$constant(-1L, shape = shape(1L))),
      drop_remainder = TRUE
    )
}

set.seed(1)
audio_ids_train <- df %>% distinct(audio_id) %>% sample_frac(0.7)
df_train <- df %>% semi_join(audio_ids_train)
df_val <- df %>% anti_join(audio_ids_train)
n_train <- nrow(df_train)
n_test <- nrow(df_val)

ds_train <- data_generator(df_train,
                           samples_per_window = samples_per_window,
                           stride_samples = stride_samples)
ds_val <- data_generator(df_val,
                         samples_per_window = samples_per_window,
                         stride_samples = stride_samples)


library(keras)
model <- keras_model_sequential()
model %>%
  layer_conv_2d(
    input_shape = c(63, 257, 1),
    filters = 32,
    kernel_size = c(3, 3),
    activation = 'relu',
    padding = "same"
  ) %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_conv_2d(filters = 64,
                kernel_size = c(3, 3),
                activation = 'relu',
                padding = "same") %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_conv_2d(filters = 128,
                kernel_size = c(3, 3),
                activation = 'relu',
                padding = "same") %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_conv_2d(filters = 256,
                kernel_size = c(3, 3),
                activation = 'relu',
                padding = "same") %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_dropout(rate = 0.2) %>%
  layer_flatten() %>%
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 3, activation = 'softmax')

model %>% compile(
  loss = loss_categorical_crossentropy,
  optimizer = optimizer_adadelta(),
  metrics = c('accuracy')
)

history <- model %>% fit(
  ds_train,
  epochs = 10,
  steps_per_epoch = trunc(n_train / batch_size),
  validation_data = ds_val,
  validation_steps = trunc(n_test / batch_size)
)

model %>% keras::save_model_hdf5("data/modelos/mod_1000ms_0001")

predictions <- predict_generator(
  model,
  ds_val,
  steps = 32
)
str(predictions)
