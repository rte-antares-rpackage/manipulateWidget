#' Function called when user clicks on the "Done" button. It stops the shiny
#' gadget and returns the final htmlwidget
#'
#' @param .expr Expression that generates a htmlwidget
#' @param controls Object created with function preprocessControls
#'
#' @return a htmlwidget
#' @noRd
onDone <- function(controller, stopApp = TRUE) {
  for (env in controller$envs$ind) {
    assign(".initial", TRUE, envir = env)
    assign(".session", NULL, envir = env)
  }
  controller$updateCharts()
  res <- controller$returnCharts()

  if (stopApp) shiny::stopApp(res)
  else return(res)
}
