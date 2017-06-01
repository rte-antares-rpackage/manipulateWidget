#' Private function that generates the general layut of the application
#'
#' @param controls Object returned by preprocessControls
#' @param ncol Number of columns in the chart area.
#' @param nrow Number of rows in the chart area.
#'
#' @return shiny tags
#'
#' @noRd
.uiLayout <- function(controls, ncol = 1, nrow = 1) {

  showSettings <- controls$nmod == 1 || length(controls$controls$shared > 0)

  menu <- tags$div(
    class="menu"
  )

  controls <- tags$div(
    class="controls"
  )

  container <- tags$div(
    class="mw-container",

    .uiMenu(controls$nmod, ncol, nrow, showSettings),
    .uiControls(controls),
    .uiChartarea(ncol, nrow)
  )
}

.uiMenu <- function(ncharts, ncol, nrow, showSettings) {
  container <- tags$div(
    class="mw-menu"
  )

  if (showSettings) {
    settingsBtn <- tags$div(
      class="mw-btn mw-btn-settings",
      icon("gears", "fa-2x"),
      tags$div(class="right-arrow")
    )
    container <- tagAppendChild(container, settingsBtn)
  }

  if (ncharts > 1) {
    container <- tagAppendChildren(container, .uiChartBtns(ncharts, nrow, ncol))
  }

  tagAppendChild(tags$div(class="mw-btn mw-btn-ok", "OK"))
}

.uiChartBtns <- function(ncharts, nrow, ncol) {
  lapply(seq_len(ncharts), function(i) {

  })
}
