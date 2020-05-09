fname = list.files("data-raw/wav_16khz_500ms/", full.names = TRUE)[20]
tamanho_dos_audios = 0.5
samples_per_window = 512L
stride_samples = 0.5
num_mel_bins = 64L
num_mfccs = 13L
sampling_rate = 16000L
seed = 1

set.seed(seed)
tamanho_dos_audios_em_ms <- tamanho_dos_audios * 1000L

stride_samples <- as.integer((1-stride_samples) * samples_per_window)
df <- tibble(fname = fname)
batch_size <- 1
lower_edge_hertz = 0
upper_edge_hertz = 2595 * log10(1 + (sampling_rate / 2) / 700)

range <- tf$range(
  samples_per_window %/% 2L,
  as.integer(tamanho_dos_audios*sampling_rate) - samples_per_window %/% 2L,
  stride_samples
)

n_fft_coefs <- (2 ^ tf$math$ceil(tf$math$log(tf$cast(samples_per_window, tf$float32)) / tf$math$log(2))/2+ 1L) %>% tf$cast(tf$int32)

n_periods <- tf$shape(range)[1] + 2L
n_coefs <- tf$constant(num_mfccs)

ds <- tensors_dataset(df) %>%
  dataset_map(function(obs) {
    wav <- tf$audio$decode_wav(tf$io$read_file(tf$reshape(obs$fname, list())), desired_channels = 1L)
    samples <- wav$audio
    samples <- samples %>% tf$transpose(perm = c(1L, 0L))
    
    stft_out <- tf$signal$stft(samples,
                               frame_length = as.integer(samples_per_window),
                               frame_step = as.integer(stride_samples))
    
    magnitude_spectrograms <- tf$abs(stft_out)
    log_magnitude_spectrograms <- tf$math$log(magnitude_spectrograms + 1e-6)
    
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
    input
  })

ds <- make_iterator_one_shot(ds)
x <- iterator_get_next(ds)
size <- x$get_shape()[[0]]
paddings = tf$constant(c(c(0L, n_periods$numpy() - 2L - size),
                         c(0L, 0L),
                         c(0L, 0L)), shape = c(3L, 2L))
x <- tf$pad(x, paddings, "CONSTANT")

# dados
x_dim <- dim(x)
x <- x %>% 
  as.array() %>%
  rray::rray_reshape(dim = c(1, prod(x_dim))) %>% 
  as.data.frame()

modelo03 <- read_rds("data/modelos_mfcc/modelo03.rds")
pred <- bind_cols(
  predict(modelo03, x, type = "prob"),
  predict(modelo03, x, type = "class")
)

