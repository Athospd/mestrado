sons_de_megascops_choliba <- warbleR::querxc("megascops choliba", download = TRUE, path = "data-raw/")


library(ggplot2)
library(tuneR)
library(dplyr)
library(viridis)
## first layer

spectrogram <- spectro(wave, f = f, plot = FALSE, ...)
frequency <- rep(spectrogram$freq, times = ncol(spectrogram$amp))
time <- rep(spectrogram$time, each = nrow(spectrogram$amp))
amplitude <- as.vector(spectrogram$amp)
df <- data.frame(time, frequency, amplitude)
ggplot(df, aes_string(x = "time", y = "frequency", z = "amplitude")) +
  xlab("Time (s)") +
  ylab("Frequency (kHz)") +
  scale_fill_continuous("Amplitude\n(dB)\n")

ggspectro_obj <- tuneR::readMP3("data-raw/mp3/Megascops-choliba-100743.mp3") %>%
  ggspectro(ovlp = 90)

ggspectro_obj +
  geom_tile(aes(fill = amplitude)) +
  stat_contour() +
  scale_fill_viridis(option = "magma")
