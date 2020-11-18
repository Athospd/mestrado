# This code should run with TensorFlow >= 1.13.
# It was designed to run in graph as well as eager modes.
# If running in eager mode only, some pieces of code can be substantially simplified.
library(keras)
library(magrittr)
library(tidyverse)
library(tensorflow)
library(tfdatasets)
library(stringr)
library(dplyr)

my_dataset_padded_batch <- function (dataset, batch_size, padded_shapes, padding_values = NULL, drop_remainder = FALSE) {
  as_tf_dataset(dataset$padded_batch(batch_size = tfdatasets:::as_integer_tensor(batch_size),
                                     padded_shapes = tfdatasets:::as_tensor_shapes(padded_shapes),
                                     padding_values = padding_values,
                                     drop_remainder = TRUE))
}

ajusta_modelo <- function(
  modelo_dir, #OK
  audio_ids_train,
  tamanho_dos_audios = 1, #OK
  samples_per_window = 512L, #OK
  stride_samples = 0.5, #OK
  batch_size = 32L, #OK
  type = c("log_magnitude", "log_mel", "mfcc")[1], #OK
  num_mel_bins = 64L, #OK
  num_mfccs = 12L, #OK
  epochs = 10, #OK
  sampling_rate = 16000L, #OK
  seed = 1
) {
  set.seed(seed)
  tamanho_dos_audios_em_ms <- tamanho_dos_audios * 1000L
  stride_samples <- as.integer((1-stride_samples) * samples_per_window)
  
  
  df <- readr::read_rds(glue::glue("data/slices_{tamanho_dos_audios_em_ms}ms_labels_by_humans.rds")) %>%
    dplyr::mutate(
      fname = fs::as_fs_path(paste0(glue::glue("data-raw/wav_16khz_{tamanho_dos_audios_em_ms}ms/"), slice_id)),
      flag = case_when(
        label %in% "Glaucidium-minutissimum" ~ 1L,
        label %in% "Strix-hylophila" ~ 2L,
        TRUE ~ 0L
      )
    )
  
  buffer_size <- nrow(df)
  
  num_categs <- as.integer(n_distinct(df$flag))
  lower_edge_hertz = 0
  upper_edge_hertz = 2595 * log10(1 + (sampling_rate / 2) / 700)
  
  range <- tf$range(
    samples_per_window %/% 2L,
    as.integer(tamanho_dos_audios*sampling_rate) - samples_per_window %/% 2L,
    stride_samples
  )
  
  n_fft_coefs <- (2 ^ tf$math$ceil(tf$math$log(tf$cast(samples_per_window, tf$float32)) / tf$math$log(2))/2+ 1L) %>% tf$cast(tf$int32)
  
  n_periods <- tf$shape(range)[1] + 2L
  
  n_coefs <- if (type == "log_magnitude") {
    n_fft_coefs
  } else if (type == "log_mel") {
    tf$constant(num_mel_bins)
  } else if (type == "mfcc") {
    tf$constant(num_mfccs)
  }
  
  data_generator <- function(df,
                             samples_per_window,
                             stride_samples,
                             samp_rate = sampling_rate) {
    
    ds <- tensor_slices_dataset(df) %>%
      dataset_shuffle(buffer_size = buffer_size) %>%
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
          linear_to_mel_weight_matrix <-
            tf$signal$linear_to_mel_weight_matrix(
              num_mel_bins,
              num_spectrogram_bins = n_fft_coefs,
              sample_rate = samp_rate,
              lower_edge_hertz = lower_edge_hertz,
              upper_edge_hertz = upper_edge_hertz
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
        
        response <- tf$one_hot(obs$flag, num_categs)
        
        if (type == "log_magnitude") {
          input <- tf$transpose(log_magnitude_spectrograms, perm = c(1L, 2L, 0L))
        } else if (type == "log_mel") {
          input <- tf$transpose(log_mel_spectrograms, perm = c(1L, 2L, 0L))
        } else if (type == "mfcc") {
          input <- tf$transpose(mfccs, perm = c(1L, 2L, 0L))
        }
        
        list(input, response)
        
      }) %>%
      dataset_repeat()
    
    ds %>%
      my_dataset_padded_batch(
        batch_size = batch_size,
        padded_shapes = list(tf$stack(list(
          n_periods, n_coefs, -1L
        )),
        tf$constant(-1L, shape = shape(1L))),
        drop_remainder = TRUE
      )
  }
  
  df_train <- df %>% semi_join(audio_ids_train, by = "audio_id")
  df_val <- df %>% anti_join(audio_ids_train, by = "audio_id")
  n_train <- nrow(df_train)
  n_test <- nrow(df_val)
  
  ds_train <- data_generator(df_train,
                             samples_per_window = samples_per_window,
                             stride_samples = stride_samples)
  ds_val <- data_generator(df_val,
                           samples_per_window = samples_per_window,
                           stride_samples = stride_samples)
  
  
  model <- keras_model_sequential()
  model %>%
    layer_conv_2d(
      input_shape = c(n_periods, n_coefs, 1),
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
    layer_dropout(rate = 0.2) %>%
    layer_flatten() %>%
    layer_dense(units = 128, activation = 'relu') %>%
    layer_dropout(rate = 0.2) %>%
    layer_dense(units = num_categs, activation = 'softmax')
  
  model %>% keras::compile(
    loss = 'categorical_crossentropy',
    optimizer = optimizer_adadelta(),
    metrics = c('accuracy')
  )
  
  history <- model %>% fit(
    ds_train,
    epochs = epochs,
    steps_per_epoch = trunc(n_train / batch_size),
    validation_data = ds_val,
    validation_steps = trunc(n_test / batch_size),
    verbose = 1
  )
  
  keras::save_model_tf(model, filepath = modelo_dir)
  model_utils <- list(
    df_train = df_train,
    df_val = df_val,
    data_generator = data_generator,
    samples_per_window = samples_per_window,
    stride_samples = stride_samples,
    history = history,
    modelo_dir = modelo_dir
  )
  readr::write_rds(model_utils, paste0(modelo_dir, ".rds"))
  return(history$metrics$val_accuracy)
}



#################################################################################################
parametros <- expand_grid(
  tamanho_dos_audios = c(0.2, 0.5, 1, 2), 
  samples_per_window = c(128L, 256L, 512L), 
  stride_samples = c(0, 0.5), 
  type = c("log_magnitude", "log_mel", "mfcc"),
  modelo_dir = "data/modelos_arquitetura2"
) %>%
  rownames_to_column("modelo_id") %>%
  mutate(
    modelo_id = str_pad(modelo_id, 4, pad = "0"),
    modelo_dir = glue::glue("{modelo_dir}/arquitetura2_{modelo_id}"),
  )

set.seed(1)
audio_ids_train <- readr::read_rds("data/audio_ids_train.rds")

ajusta_modelo <- ajusta_modelo %>% purrr::partial(audio_ids_train = audio_ids_train) %>% purrr::possibly(NA_real_, quiet = TRUE)

# ajuste dos modelos ###########################################
# library(furrr)
# plan(multiprocess, workers = 6)
# plan(sequential)
modelos <- parametros %>%
  filter(tamanho_dos_audios %in% 1, type == "mfcc", stride_samples == 0.5, samples_per_window == 512) %>%
  group_by(modelo_id) %>%
  nest_legacy(.key = "parametros") %>%
  mutate(
    val_accuracy = purrr::map2(
      modelo_id,
      parametros,
      ~{
        tensorflow::tf$random$set_seed(0L)
        do.call(ajusta_modelo, .y)
      }
      # .options = future_options(
      #   globals = c("audio_ids_train", "ajusta_modelo", "my_dataset_padded_batch"),
      #   packages = c("keras",
      #                "magrittr",
      #                "tidyverse",
      #                "tensorflow",
      #                "tfdatasets",
      #                "stringr",
      #                "dplyr")
      # ),
      # .progress = TRUE
    )
  )

readr::write_rds(modelos, path = glue::glue("data/modelos_arquitetura2/arquitetura2_1000ms.rds"))
