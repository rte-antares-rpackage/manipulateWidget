#' Function called when user clicks on the "Done" button. It stops the shiny
#' gadget and returns the final htmlwidget
#'
#' @param .expr Expression that generates a htmlwidget
#' @param controls Object created with function preprocessControls
#'
#' @return a htmlwidget
#' @noRd
onDone <- function(controller, .return = function(w, e) {w}) {
  for (env in controller$envs) {
    assign(".initial", TRUE, envir = env)
    assign(".session", NULL, envir = env)
  }
  controller$updateCharts()

  shiny::stopApp(controller$returnCharts())
}
