% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_prep.R
\name{tidy_audio_dir}
\alias{tidy_audio_dir}
\title{Tidy MP3 or Wave}
\usage{
tidy_audio_dir(
  audio_files,
  dest_dir = tempdir(),
  sample_rate = 16000,
  bits = 16,
  parallel = 1
)
}
\arguments{
\item{audio_files}{character vector with the full path for the mp3 files.}

\item{dest_dir}{optional atomic character indicating the path where to store 
the files. Default is a tempdir(). Set NULL if you want the dest dir be the
same as origin (warning: it will overwrite the original wave files!)}

\item{sample_rate}{integer. Sample rate.}

\item{bits}{integer.}
}
\value{
a character vector of the same size of audio_files with the file names of the transformed wavs.
}
\description{
Apply transformations for modelling ready audio files.
}
\details{
The transformations are:

- mp3 to wav
- normalization
- stereo to mono
}
\examples{

mp3_dir <- system.file("mp3_sample", package = "mestrado")
mp3_files <- list.files(mp3_dir, full.names = TRUE)

wav_files <- tidy_audio(mp3_files)
}
