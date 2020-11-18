library(tidyverse)
library(torch)
library(torchaudio)
library(mestrado)
library(tensorflow)

create_mfcc_dataset <- function(
  tamanho_dos_audios,
  samples_per_window = 512L,
  stride_samples = 0.5,
  num_mel_bins = 64L,
  num_mfccs = 13L,
  sampling_rate = 16000L,
  seed = 1
) {
  
  tamanho_dos_audios = 1
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
  batch_size <- 1
  num_categs <- as.integer(n_distinct(df$flag))
  lower_edge_hertz <- 0
  upper_edge_hertz <- linear_to_mel_frequency(sampling_rate / 2)
  
  range <- torch_arange(
    samples_per_window %/% 2L,
    as.integer(tamanho_dos_audios*sampling_rate) - samples_per_window %/% 2L,
    stride_samples
  )
  
  n_fft_coefs <- as.integer(2^ceiling(log(samples_per_window)/log(2))/2 + 1)
  n_periods <- length(range) + 2
  
  # read mp3
  obs <- df[1, ]
  wav <- tuneR::readWave(as.character(obs$fname)) %>% tidy_audio(sample_rate = sampling_rate)
  
  stft_out <- spectrogram(
    torch_tensor(as.numeric(wav@left)/(2^(wav@bit - 1))), 
    n_fft = samples_per_window,
    hop_length = (stride_samples),
    window = torch_hann_window(samples_per_window, dtype = torch:::dtype_from_string("float")),
    center = FALSE, 
    normalized = FALSE, 
    power = 2
  )
  magnitude_spectrograms <- sqrt(stft_out)
  
  plot((as.numeric(magnitude_spectrograms$t())), as.numeric(tf$squeeze(magnitude_spectrograms2)))
  lm(sqrt(as.numeric(stft_out$t())) ~ as.numeric(tf$squeeze(magnitude_spectrograms2)))
  
  log_magnitude_spectrograms <- log(magnitude_spectrograms + 1e-6)
  
  # plot_spectrogram_matrix(log_magnitude_spectrograms)
  
  linear_to_mel_weight_matrix <- create_fb_matrix( 
    n_mels = num_mel_bins,
    n_freqs = n_fft_coefs,
    sample_rate = sampling_rate,
    f_min = lower_edge_hertz,
    f_max = upper_edge_hertz
  )
  
  mel_spectrograms <- torch_mm(
    magnitude_spectrograms$transpose(1L, 2L),
    linear_to_mel_weight_matrix
  )
  
  plot_spectrogram_matrix(magnitude_spectrograms)
  plot_spectrogram_matrix(tf$squeeze(magnitude_spectrograms2))
  plot_spectrogram_matrix(linear_to_mel_weight_matrix)
  plot_spectrogram_matrix(tf$squeeze(linear_to_mel_weight_matrix2))
  plot_spectrogram_matrix(mel_spectrograms)
  plot_spectrogram_matrix(tf$squeeze(mel_spectrograms2))
  plot(as.numeric(mel_spectrograms), (as.numeric(tf$squeeze(mel_spectrograms2))))
  abline(0, 1, col = "red")
  hist(as.numeric(mel_spectrograms) - (as.numeric(tf$squeeze(mel_spectrograms2))*152365.85 + 18.28), breaks = 100)
  lm(as.numeric(mel_spectrograms) ~ as.numeric(tf$squeeze(mel_spectrograms2)))
  
  log_mel_spectrograms <- log(mel_spectrograms + 1e-6)
  plot_spectrogram_matrix(log_mel_spectrograms)
  plot_spectrogram_matrix(tf$squeeze(log_mel_spectrograms2))
  plot(as.numeric(log_mel_spectrograms), as.numeric(tf$squeeze(log_mel_spectrograms2)))
  
  dct_mat <- create_dct(n_mfcc = num_mfccs, n_mels = num_mel_bins, norm = 'ortho')
  
  mfccs <- torch_matmul(log_mel_spectrograms,dct_mat)
  
  
  mfccs2 <- tf$signal$mfccs_from_log_mel_spectrograms(log_mel_spectrograms2)[ , , 1:num_mfccs]
  
  mfcc3 <- tuneR::melfcc(samples = wav, wintime = samples_per_window/16000, hoptime = samples_per_window/16000/2, sr = 16000, numcep = 13)
  
  plot(as.numeric(mfccs), as.numeric(tf$squeeze(mfccs2)))
  plot(as.numeric(mfccs), as.numeric((mfcc3[nrow(mfcc3):1, ncol(mfcc3):1])))
  plot(as.numeric(mfcc3), as.numeric(tf$squeeze(mfccs2)))
  
  plot_spectrogram_matrix(dct_mat)
  plot_spectrogram_matrix(mfccs)
  plot_spectrogram_matrix(tf$squeeze(mfccs2))
  plot_spectrogram_matrix(as.array(tf$squeeze(mfccs2))/as.array(mfccs))
  
  response <- torch::nnf_one_hot(obs$flag, num_categs)
  input <- response
  
  
  # full mfcc dataset
  obs <- lapply(df[1,], tf$convert_to_tensor)
  ds <- tfdatasets::tensor_slices_dataset(df) %>%
    tfdatasets::dataset_map(function(obs) {
      obs2 <- lapply(df[1,], tf$convert_to_tensor)
      wav2 <- tf$audio$decode_wav(tf$io$read_file(tf$reshape(obs2$fname, list())), desired_channels = 1L)
      samples <- wav2$audio
      samples <- samples %>% tf$transpose(perm = c(1L, 0L))
      
      stft_out2 <- tf$signal$stft(samples,
                                 frame_length = as.integer(samples_per_window),
                                 frame_step = as.integer(stride_samples))
      
      magnitude_spectrograms2 <- tf$abs(stft_out2)
      log_magnitude_spectrograms2 <- tf$math$log(magnitude_spectrograms2 + 1e-6)
      # plot_spectrogram_matrix(t(array(as.array(log_magnitude_spectrograms), dim = c(5843, 257))))
      response2 <- tf$one_hot(obs2$flag, num_categs)
      
      input2 <- tf$transpose(log_magnitude_spectrograms2, perm = c(1L, 2L, 0L))
      
      linear_to_mel_weight_matrix2 <- tf$signal$linear_to_mel_weight_matrix(
        num_mel_bins,
        num_spectrogram_bins = n_fft_coefs,
        sample_rate = sampling_rate,
        lower_edge_hertz = lower_edge_hertz,
        upper_edge_hertz = upper_edge_hertz
      )
      
      mel_spectrograms2 <- tf$tensordot(magnitude_spectrograms2,
                                        linear_to_mel_weight_matrix2,
                                       1L)
      
      
      log_mel_spectrograms2 <- tf$math$log(mel_spectrograms2 + 1e-6)
      mfccs2 <- tf$signal$mfccs_from_log_mel_spectrograms(log_mel_spectrograms2)[ , , 1:num_mfccs]
      input2 <- tf$transpose(mfccs2, perm = c(1L, 2L, 0L))
      # fname <- tf$reshape(obs$fname, list())
      slice_id2 <- tf$reshape(obs$slice_id, list())
      list(input2, response2, slice_id2)
    }) %>%
    dataset_batch(batch_size, TRUE)
  
  
  ds <- make_iterator_one_shot(ds)
  .x = iterator_get_next(ds)
  pb <- progress::progress_bar$new(total = nrow(df)/batch_size -1)
  for(i in seq(nrow_df/batch_size -1)) {
    pb$tick()
    next_ <- iterator_get_next(ds)
    size <- next_[[1]]$get_shape()[[1]]
    paddings = tf$constant(c(c(0L, 0L), 
                             c(0L, n_periods$numpy() - 2L - size),
                             c(0L, 0L),
                             c(0L, 0L)), shape = c(4L, 2L))
    browser()
    if(dim(next_[[1]])[2] < 124) next_[[1]] <- tf$pad(next_[[1]], paddings, "CONSTANT")
    .x = list(
      tf$concat(list(.x[[1]], next_[[1]]), 0L),
      tf$concat(list(.x[[2]], next_[[2]]), 0L),
      tf$concat(list(.x[[3]], next_[[3]]), 0L)
    )
  }
  
  .x = list(
    .x[[1]]$numpy(),
    .x[[2]]$numpy(),
    map_chr(.x[[3]]$numpy(), as.character)
  )
  
  ds_path = glue::glue("data/mfcc_{tamanho_dos_audios_em_ms}ms.rds")
  write_rds(.x, path = ds_path)
  
  ds_path
}
create_mfcc_dataset(2)
