#' plot_pixel_matrix
#' 
#' Plot a pixel matrix with magma palette. Made to plot spectrograms.
#'
#' @return
#' @export
plot_pixel_matrix <- function(pixel_matrix, title = "") {
  pixel_matrix_as_array <- as.array(pixel_matrix)
  image(
    t(pixel_matrix_as_array),
    col = viridis::viridis(n = 257,  option = "magma")  
  ) 
}
