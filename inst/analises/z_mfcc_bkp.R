library(keras)
library(magrittr)
library(tidyverse)
library(tensorflow)
library(tfdatasets)
library(stringr)
library(dplyr)
library(numbers)

create_mfcc_dataset <- function(
  tamanho_dos_audios,
  samples_per_window = 512L,
  stride_samples = 0.5,
  num_mel_bins = 64L,
  num_mfccs = 13L,
  sampling_rate = 16000L,
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
  
  nrow_df <- nrow(df)
  divisors_of_nrow <- divisors(nrow_df)
  batch_size <- 1#as.integer(max(divisors_of_nrow[divisors_of_nrow <= 128]))
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
  
  # full mfcc dataset
  n_coefs <- tf$constant(num_mfccs)
  
  ds <- tensor_slices_dataset(df) %>%
    dataset_map(function(obs) {
      wav <- tf$audio$decode_wav(tf$io$read_file(tf$reshape(obs$fname, list())), desired_channels = 1L)
      samples <- wav$audio
      samples <- samples %>% tf$transpose(perm = c(1L, 0L))
      
      stft_out <- tf$signal$stft(samples,
                                 frame_length = as.integer(samples_per_window),
                                 frame_step = as.integer(stride_samples))
      
      magnitude_spectrograms <- tf$abs(stft_out)
      log_magnitude_spectrograms <- tf$math$log(magnitude_spectrograms + 1e-6)
      
      response <- tf$one_hot(obs$flag, num_categs)
      
      input <- tf$transpose(log_magnitude_spectrograms, perm = c(1L, 2L, 0L))
      
      magnitude_spectrograms <- tf$abs(stft_out)
      linear_to_mel_weight_matrix <- tf$signal$linear_to_mel_weight_matrix(
        num_mel_bins,
        num_spectrogram_bins = n_fft_coefs,
        sample_rate = sampling_rate,
        lower_edge_hertz = lower_edge_hertz,
        upper_edge_hertz = upper_edge_hertz
      )
      mel_spectrograms <- tf$tensordot(magnitude_spectrograms,
                                       linear_to_mel_weight_matrix,
                                       1L)
      log_mel_spectrograms <- tf$math$log(mel_spectrograms + 1e-6)
      mfccs <- tf$signal$mfccs_from_log_mel_spectrograms(log_mel_spectrograms)[ , , 1:num_mfccs]
      input <- tf$transpose(mfccs, perm = c(1L, 2L, 0L))
      
      list(input, response)
    }) %>%
    dataset_batch(batch_size, TRUE)
  
  ds <- make_iterator_one_shot(ds)
  .x = iterator_get_next(ds)
  pb <- progress::progress_bar$new(total = nrow(df)/batch_size -1)
  tmp <- tempdir()
  for(i in seq(nrow_df/batch_size -1)) {
    pb$tick()
    next_ <- iterator_get_next(ds)
    size <- next_[[1]]$get_shape()[[1]]
    paddings = tf$constant(c(c(0L, 0L), 
                             c(0L, n_periods$numpy() - 2L - size),
                             c(0L, 0L),
                             c(0L, 0L)), shape = c(4L, 2L))
    next_[[1]] <- tf$pad(next_[[1]], paddings, "CONSTANT")
    .x = list(
      tf$concat(list(.x[[1]], next_[[1]]), 0L),
      tf$concat(list(.x[[2]], next_[[2]]), 0L)
    )
    
    if(i %% 100000 == 0) {
      pt = parte = i %/% 100000
      cat(pt)
      
      .x = list(
        .x[[1]]$numpy(),
        .x[[2]]$numpy()
      )
      
      ds_path = glue::glue("{tmp}/mfcc_{tamanho_dos_audios_em_ms}ms_{pt}.rds")
      write_rds(.x, path = ds_path)
      
      .x <- NULL
      gc()
    }
  }
  
  .x = list(
    .x[[1]]$numpy(),
    .x[[2]]$numpy()
  )
  
  ds_path = glue::glue("{tmp}/mfcc_{tamanho_dos_audios_em_ms}ms_final.rds")
  write_rds(.x, path = ds_path)
  
  ds_arqs = list.files(tmp, full.names = TRUE)
  for(arq in ds_arqs) {
    next_ <- read_rds(arq)
    .x = list(
      rray::rray_bind(list(.x[[1]], next_[[1]]), 1L),
      rray::rray_bind(list(.x[[2]], next_[[2]]), 1L)
    )
  }
  
  ds_path = glue::glue("data/mfcc_{tamanho_dos_audios_em_ms}ms.rds")
  write_rds(.x, path = ds_path)
  
  ds_path
}
create_mfcc_dataset(1)
create_mfcc_dataset(0.5)
create_mfcc_dataset(0.2)
