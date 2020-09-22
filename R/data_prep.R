#' Tidy MP3 or Wave
#'
#' Apply transformations for modelling ready audio files.
#' 
#' The transformations are:
#' 
#' - mp3 to wav
#' - normalization
#' - stereo to mono
#' 
#' @param audio_files character vector with the full path for the mp3 files.
#' @param dest_dir optional atomic character indicating the path where to store 
#' the files. Default is a tempdir(). Set NULL if you want the dest dir be the
#' same as origin (warning: it will overwrite the original wave files!)
#' @param sample_rate integer. Sample rate.
#' @param bits integer. 
#'
#' @return a character vector of the same size of audio_files with the file names of the transformed wavs.
#'
#' @export
#'
#' @examples
#' 
#' mp3_dir <- system.file("mp3_sample", package = "mestrado")
#' mp3_files <- list.files(mp3_dir, full.names = TRUE)
#' 
#' wav_files <- tidy_audio(mp3_files)
tidy_audio_dir <- function(audio_files, dest_dir = tempdir(), sample_rate = 16000, bits = 16, parallel = 1) {
  # if dir is set, make sure it exists
  if(!is.null(dest_dir)) {
    dest_dir <- paste0(dest_dir, "/")

    if(!dir.exists(dest_dir)) {
      dir.create(dest_dir)
    }
  } 
  # fix windows '\\' issue
  dest_dir <- gsub("\\\\", "/", dest_dir)
  
  new_audio_files <- character(length(audio_files))
  pb <- progress::progress_bar$new(total = length(audio_files))
  for(i in seq_along(audio_files)) {
    pb$tick()
    audio_file <- audio_files[i]
    file_ext <- tools::file_ext(audio_file)
    
    # load audio file
    if(file_ext == "mp3"){
      wav <- tuneR::readMP3(audio_file)
    } else {
      wav <- tuneR::readWave(audio_file)
    }
    
    # mp3 to wav in the name file
    new_audio_file <- gsub("mp3$", "wav", audio_file)
    
    # update name for the right dest_dir to store
    if(!is.null(dest_dir)) new_audio_file <- gsub("^.*/", dest_dir, new_audio_file)
    
    wav <- tidy_audio(wav, sample_rate = sample_rate, bits = bits)
    
    # write wave in the disk
    tuneR::writeWave(wav, new_audio_file)
    new_audio_files[i] <- new_audio_file
  }
  return(new_audio_files)
}

#' Tidy MP3 or Wave
#'
#' Apply transformations on an wav object for modelling purposes.
#' 
#' The transformations are:
#' 
#' - mp3 to wav
#' - normalization
#' - stereo to mono
#' 
#' @param wav Wave object. Usualy returned by \code{\link[tuneR]{readMP3}}/\code{\link[tuneR]{readWave}}.
#'
#' @return a transformed Wave object.
#'
#' @export
#'
#' @examples
#' 
#' library(tuneR)
#' mp3_dir <- system.file("mp3_sample", package = "mestrado")
#' mp3_files <- list.files(mp3_dir, full.names = TRUE)
#' wav <- readMP3(mp3_files[1])
#' 
#' transformed_wav <- tidy_audio(wav)
tidy_audio <- function(wav, sample_rate = 16000, bits = 16) {
  # normalize
  wav <- tuneR::normalize(wav, unit = as.character(bits))
  
  # downsample
  if(wav@samp.rate > sample_rate) {
    wav <- tuneR::downsample(wav, sample_rate)
  }
  
  wav@left <- as.numeric(wav@left) %>% tidyr::replace_na(0)
  wav@right <- as.numeric(wav@right) %>% tidyr::replace_na(0)
  
  if(wav@stereo) wav <- wav %>% tuneR::mono(which = "both")
  
  wav@left <- as.integer(wav@left)
  wav@right <- as.integer(wav@right)
  
  # to mono
  wav <- tuneR::mono(wav)
  
  wav
}


#' see this issue https://github.com/rstudio/tensorflow/issues/402
#' sudo apt install sox
identity_resampling <- function(orig, dest = orig) {
  system(glue::glue("sox {orig} -r 16000 {dest}"))
}


