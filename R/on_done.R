#' Function called when user clicks on the "Done" button. It stops the shiny
#' gadget and returns the final htmlwidget
#'
#' @param .expr Expression that generates a htmlwidget
#' @param controls Object created with function preprocessControls
#'
#' @return a htmlwidget
#' @noRd
onDone <- function(controller, .return = function(w, e) {w}, nrow = NULL, ncol = NULL) {
  for (env in controller$envs) {
    assign(".initial", TRUE, envir = env)
    assign(".session", NULL, envir = env)
  }
  controller$updateCharts()

  shiny::stopApp(mwReturn(controller$charts, .return, controller$envs, nrow, ncol))
}

#' Function that takes a list of widgets and returns the first one if there is
#' only one or a combinedWidget with all widgets combined.
#'
#' @param widgets list of htmlwidgets
#'
#' @return a htmlwidget
#' @noRd
mwReturn <- function(widgets, .return, envs, nrow = NULL, ncol = NULL) {
  if (length(widgets) == 1) {
    finalWidget <- widgets[[1]]
  } else {
    finalWidget <- combineWidgets(list = widgets, nrow = nrow, ncol = ncol)
  }
  .return(finalWidget, envs)
}
