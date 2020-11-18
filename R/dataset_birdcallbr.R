#' Birdcall Brazil Dataset
#'
#' @param root  (str): Path to the directory where the dataset is found or downloaded.
#' @param audio_duration (dbl): Duration in seconds of the audio. Allowed values are 1, 2 or 5. (default: 5)
#' @param train (bool, optional): If True, creates dataset from `audio_ids_train.rds`, otherwise from `audio_ids_test.rds`.
#' @param url  (str, optional): The URL to download the dataset from, or the type of the dataset to dowload.
#'            Allowed type values are ``"birdcallbr_v1"`` (default: ``"birdcallbr_v1"``)
#' @param folder_in_archive  (str, optional): The top-level directory of the dataset.  (default: ``"BirdcallBR"``)
#' @param download  (bool, optional): Whether to download the dataset if it is not found at root path.  (default: ``FALSE``).
#'
#' @export
birdcallbr_dataset <- torch::dataset(
  "BirdcallBR",
  
  .CHECKSUMS = list(),
  
  initialize = function(
    root,
    audio_duration = 5,
    train = TRUE,
    url = "birdcallbr_v1",
    folder_in_archive = "BirdcallBR",
    download = FALSE
  ) {
    
    self$URL <- url
    self$FOLDER_IN_ARCHIVE <- folder_in_archive
    self$DURATION <- 1000*audio_duration # in ms
    
    if(url %in% c(
      "birdcallbr_v1"
    )
    ) {
      base_url = ""
      ext_archive = ".zip"
      url = file.path(base_url, paste0(url, "_", self$DURATION,"ms", ext_archive))
    }
    
    basename = basename(url)
    archive = file.path(root, basename)
    
    basename = sub(ext_archive, "", basename, fixed = TRUE)
    folder_in_archive = file.path(folder_in_archive, basename)
    
    self$.path = file.path(root, folder_in_archive)
    
    if(download) {
      if(!fs::is_dir(self$.path)) {
        if(!fs::is_file(archive)) {
          checksum = self$.CHECKSUMS[[url]]
          download_url(url = url, destfile = archive, checksum = checksum)
        }
        torchaudio::extract_archive(archive, self$.path)
      }
    }
    
    audio_ids <- if(train) {
      readRDS(file.path(self$.path, "audio_ids_train.rds"))$audio_id
    } else {
      readRDS(file.path(self$.path, "audio_ids_test.rds"))$audio_id
    }
    
    walker = readRDS(file.path(self$.path, paste0("wavs_", self$DURATION, "ms_labels_by_humans.rds")))
    self$.walker = walker[walker$audio_id %in% audio_ids, ]
    self$labels = unique(self$.walker$label)
  },
  
  .getitem = function(n) {
    force(n)
    if(length(n) != 1 || n <= 0) value_error("n should be a single integer greater than zero.")
    wave_slice = self$.walker[n, ]
    return(self$load_birdcallbr_item(wave_slice, self$.path, self$DURATION))
  },
  
  .length = function() {
    nrow(self$.walker)
  },
  
  load_birdcallbr_item = function(wave_slice, path, audio_duration) {
    if(nrow(wave_slice) != 1) value_error("nrow(wave_slice) should be 1.")
    filepath = file.path(path, paste0("wavs_", audio_duration, "ms"), wave_slice$slice_id)
    label = wave_slice$label
    label_one_hot = self$one_hot_encode(label)
    
    # Load audio
    waveform_and_sample_rate = torchaudio::torchaudio_load(filepath, normalization = FALSE)
    waveform = waveform_and_sample_rate[[1]][1]$unsqueeze(1)
    sample_rate = waveform_and_sample_rate[[2]]
    
    return(list(waveform = waveform,
                slice_id = wave_slice$slice_id,
                filepath = filepath,
                sample_rate = sample_rate,
                label = label,
                label_one_hot = label_one_hot))
  },
  
  one_hot_encode = function(label) {
    one_hot = torch::torch_zeros((length(self$labels)))
    one_hot[which(self$labels == label)] = 1
    return(one_hot)
  },
  
  show_spec = function(n) {
    mestrado::plot_pixel_matrix(torchaudio::transform_spectrogram()(self[n]$waveform)$squeeze(1))
  },
  
  show_melspec = function(n) {
    mestrado::plot_pixel_matrix(torchaudio::transform_mel_spectrogram()(self[n]$waveform)$squeeze(1))
  },
  
  show_waveform = function(n) {
    plot(self[n]$waveform$squeeze(1), type = "l", col = "royalblue")
  }
)
