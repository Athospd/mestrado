
#' visualizadorDeSons
#'
#' Launch shiny example application using shinyhttr::progress_bar. This app also uses module to show that it works with it too.
#'
#' @param display.mode The mode in which to display the example. Defaults to showcase, but may be set to normal to see the example without code or commentary.
#'
#' @export
visualizadorDeSons <- function(display.mode = "showcase") {
  appDir <- system.file("shiny-apps", "visualizadorDeSons", package = "mestrado")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `visualizadorDeSons`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = display.mode)
}
