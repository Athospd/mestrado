% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_labeling.R
\name{label_slices}
\alias{label_slices}
\title{Label Slices of Wave Files}
\usage{
label_slices(slices_dir, annotations, pattern = NULL)
}
\arguments{
\item{slices_dir}{atomic character. Path to dir where the slices are (see \code{\link{slice_wavs}}).}

\item{pattern}{atomic character. A regex pattern to select files from 'slices_dir'. Useful to filter 
out just specifc types of labels (see examples).}

\item{annotations_dir}{atomic character or a tibble. If character, path to dir where the annotations 
are. If tibble, a tibble created by \code{\link{tidy_annotations}}.}
}
\value{
tibble of slices with label column.
}
\description{
Plug the labels from a set of annotations (made by \code{\link[wavesurfer]{annotator_app}}) in 
a dataset of slices (made by \code{\link{slice_wavs}}).
}
\examples{

slices_dir <-  system.file("wav_sample_slices_1000ms", package = "mestrado")
annotations_dir <-  system.file("annotations", package = "mestrado")
slices_1000ms_labels <- label_slices(
  slices_dir, 
  annotations_dir, 
  pattern = "Glaucidium|Megascops-atricapilla"
)
}
