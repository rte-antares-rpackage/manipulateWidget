#' Function called when user clicks on the "Done" button. It stops the shiny
#' gadget and returns the final htmlwidget
#'
#' @param .expr Expression that generates a htmlwidget
#' @param controls Object created with function preprocessControls
#'
#' @return a htmlwidget
#' @noRd
onDone <- function(.expr, controls) {
  widgets <- lapply(controls$env$ind, function(e) {
    assign(".initial", TRUE, envir = e)
    assign(".session", NULL, envir = e)
    eval(.expr, envir = e)
  })

  shiny::stopApp(mwReturn(widgets))
}

#' Function that takes a list of widgets and returns the first one if there is
#' only one or a combinedWidget with all widgets combined.
#'
#' @param widgets list of htmlwidgets
#'
#' @return a htmlwidget
#' @noRd
mwReturn <- function(widgets) {
  if (length(widgets) == 1) {
    return(widgets[[1]])
  } else {
    return(combineWidgets(list = widgets))
  }
}
