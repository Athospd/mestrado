#' plot_pixel_matrix
#' 
#' Plot a pixel matrix with magma palette. Made to plot spectrograms.
#' 
#' @param pixel_matrix a 2D array or torch tensor.
#' @param title text to put as the image title.
#'
#' @return
#' @export
plot_pixel_matrix <- function(pixel_matrix, title = "") {
  pixel_matrix_as_array <- as.array(pixel_matrix)
  image(
    t(pixel_matrix_as_array), xlab = title,
    col = viridis::viridis(n = 257,  option = "magma")  
  ) 
}


#' Images Tensor to Coordinate data.frame
#' 
#' Converts a batch of (image) torch tensors (B x C x H x W) into a data.frame with xy coordinates along with channels (x, y, c1, ..., cn).
#' Useful along with ggplot2 plotting.
#' 
#' @param image_tensors tensor with dimension (B x C x H x W).
#'
#' @return data.frame with B*H*W rows and 3 + C columns.
#' @examples 
#' 
#' imgs <- torch::torch_tensor(1:prod(2*3*3*4))$reshape(c(2,3,3,4))
#' image_tensors_to_tbl(imgs)
#' 
#' @export
image_tensors_to_tbl <- function(image_tensors) {
  while(length(dim(image_tensors)) < 4) image_tensors <- image_tensors$unsqueeze(1)
  
  c(b, c, h, w) %<-% dim(image_tensors)
  channels <- image_tensors$permute(c(1,4,3,2))$reshape(c(b*h*w, c)) %>% 
    torch::as_array() %>% 
    as.data.frame()
  
  names(channels) <- sub(pattern = "V", "c", names(channels))
  
  ixy <- expand.grid(y = 1:h, x = 1:w, i = 1:b)[ ,c("i", "y", "x")]
  
  cbind(ixy, channels)
}

#' Plot Pixel Grids
#' 
#' Helper function to plot images from "image tensors" data.frames. Designed to work with [mestrado::image_tensors_to_tbl].
#' 
#' @param image_tensor_tbl image tensor tibble. Usually a output from [mestrado::image_tensors_to_tbl].
#' @param nrow facet_wrap's nrow.
#' @param ncol facet_wrap's ncol.
#' @param x bare name. Column of coordinate X.
#' @param y bare name. Column of coordinate Y.
#' @param channels <tidy-select> columns of channels. It must select 3 or less columns (for R, G and B). If less than 3, channels will be recycled.
#' @param label expr. Column or expression to put as title for each image.
#' @param grid_tickness numeric. Tickness of the pixel grid.
#' @param grid_colour character. Colour of the pixel grid.
#' @param ... arguments passed to facet_wrap.
#' 
#' @export
ggpixelgrid <- function(image_tensor_tbl, nrow = NULL, ncol = NULL, x = x, y = y, channels = dplyr::starts_with("c"), label = i, grid_tickness = 0, grid_colour = "grey", ...) {
  # channels_vars <- rlang::expr_deparse(channels)
  channels_vars <- tidyselect::vars_select(names(image_tensor_tbl), {{channels}})
  # if(length(channels_vars) < 3) message("Less than 3 channels found, recycling.")
  if(length(channels_vars) > 3) message("More than 3 channels found, using ", paste(rep_len(channels_vars, 3), collapse = ", "), " as r,g and b respectively.")
  channel_columns <- image_tensor_tbl[,rep_len(channels_vars, 3)]
  pixel_colours <- rlang::exec(grDevices::rgb, channel_columns, maxColorValue = if(max(channel_columns) > 1) 255 else 1)
  
  image_tensor_tbl %>%
    dplyr::mutate(label = {{label}}) %>%
    ggplot2::ggplot() +
    ggplot2::geom_tile(ggplot2::aes(x = {{x}}, y = rev({{y}})), fill = pixel_colours, colour = grid_colour, size = grid_tickness, show.legend = FALSE) +
    ggplot2::facet_wrap(facets = "label", nrow = nrow, ncol = ncol, ...) +
    ggplot2::theme_void(12) +
    ggplot2::coord_equal() +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::theme(
      axis.title.x = ggplot2::element_text(),
      axis.title.y = ggplot2::element_text(angle = 90)
    )
}


batch <- function(torch_dataset, n) {
  
}