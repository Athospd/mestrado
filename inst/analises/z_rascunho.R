dados <- tibble(
  arquivo = list.files("data-raw/mp3_originais/", pattern = "\\.mp3$", full"")
)


## first layer
library(seewave)
library(magrittr)
wave1 <- tuneR::readMP3("data-raw/mp3_originais/Glaucidium-minutissimum-1066225.mp3")
wave2 <- tuneR::readMP3("data-raw/mp3_originais/Glaucidium-minutissimum-1177240.mp3")
spectrogram1 <- spectro(wave1, plot = FALSE, ovlp = 50)
spectrogram2 <- spectro(wave2, plot = FALSE, ovlp = 50)
frequency1 <- rep(spectrogram1$freq, times = ncol(spectrogram1$amp))
time1 <- rep(spectrogram1$time, each = nrow(spectrogram1$amp))
amplitude1 <- as.vector(spectrogram1$amp)
df1 <- data.frame(time1, frequency1, amplitude1)

library(ggplot2)
ggplot(df1, aes_string(x = "time1", y = "frequency1", z = "amplitude1")) +
  xlab("Time (s)") +
  ylab("Frequency (kHz)") +
  geom_tile() +
  scale_fill_continuous("Amplitude\n(dB)\n")

