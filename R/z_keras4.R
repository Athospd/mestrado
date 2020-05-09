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


df <- readr::read_rds("data/slices_500ms_labels_by_humans.rds") %>%
  mutate(
    fname = fs::as_fs_path(paste0("data-raw/wav_16khz_500ms/", slice_id)),
    flag = if_else(label %in% "Glaucidium-minutissimum", 1L, 0L)
  )

buffer_size <- nrow(df)

type = "log_magnitude"

samples_per_window <- 512L
stride_samples <-  as.integer(samples_per_window/2)
lower_edge_hertz <- 0
upper_edge_hertz <- 2595 * log10(1 + (16000 / 2) / 700)
num_mel_bins <- 64L
num_mfccs <- 13L

samples_per_window = 512
stride_samples = 256
sampling_rate = 16000

n_periods <-tf$shape(
  tf$range(
    samples_per_window %/% 2L,
    8000L - samples_per_window %/% 2L,
    stride_samples
  )
)[1] + 1L

n_fft_coefs <- (2 ^ tf$math$ceil(tf$math$log(
  tf$cast(samples_per_window, tf$float32)
) / tf$math$log(2)) / 2 + 1L) %>% tf$cast(tf$int32)

ds <- tfdatasets::tensors_dataset(df)

ds <- ds %>%
  dataset_map(function(obs) {
    wav <- tf$audio$decode_wav(tf$io$read_file(tf$reshape(obs$fname, list())))
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
          as.integer(num_mel_bins),
          num_spectrogram_bins=257L,
          sample_rate=16000L,
          lower_edge_hertz=as.integer(lower_edge_hertz),
          upper_edge_hertz=as.integer(upper_edge_hertz)
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

    response <- tf$one_hot(obs$flag, 30L)

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
      n_periods, n_coefs,-1L
    )),
    tf$constant(-1L, shape = shape(1L))),
    drop_remainder = TRUE
  )


set.seed(1)
audio_ids_train <- df %>% distinct(audio_id) %>% sample_frac(0.7)
df_train <- df %>% semi_join(audio_ids_train)
df_val <- df %>% anti_join(audio_ids_train)
n_train <- nrow(df_train)
n_test <- nrow(df_val)

ds_train <- dataset_map(df_train,
                        data_prep)
ds_val <- data_generator(df_val,
                         samples_per_window = samples_per_window,
                         stride_samples = stride_samples)


library(keras)
model <- keras_model_sequential()
model %>%
  layer_conv_2d(
    input_shape = c(62, 257, 1),
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
  layer_dense(units = 30, activation = 'softmax')

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



predictions <- predict_generator(
  model,
  ds_val,
  steps = 32
)
str(predictions)
