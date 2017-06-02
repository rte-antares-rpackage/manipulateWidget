#' Private function that generates the general layout of the application
#'
#' @param controls Object returned by preprocessControls
#' @param ncol Number of columns in the chart area.
#' @param nrow Number of rows in the chart area.
#'
#' @return shiny tags
#'
#' @noRd
.uiLayout <- function(controls, nrow = 1, ncol = 1, outputFun = NULL,
                      okBtn = TRUE, updateBtn = FALSE) {

  showSettings <- controls$nmod == 1 || length(controls$controls$shared > 0)

  menu <- tags$div(
    class="menu"
  )

  controls <- tags$div(
    class="controls"
  )

  container <- tags$div(
    class="mw-container",
    .uiMenu(controls$nmod, nrow, ncol, showSettings, okBtn, updateBtn),
    .uiControls(controls),
    .uiChartarea(controls$nmod, nrow, ncol, outputFun)
  )
}

.uiControls <- function(controls) {
   controls <- c(list(controls$controls$shared), controls$controls$ind)
   controls <- unname(lapply(controls, mwControlsUI))
   controls$class <- "mw-input-container"
   do.call(tags$div, controls)
}

.uiChartarea <- function(ncharts, nrow, ncol, outputFun) {
  outputEls <- lapply(seq_len(nrow * ncol), function(i) {
    if (i > ncharts) return(tags$div())
    outputId <- paste0("output", i)
    if (is.null(outputFun)) shiny::htmlOutput(outputId, style="width:100%;height:100%;", class="mw-chart")
    else .outputFun(.outputId, width = "100%", height = "100%", class = "mw-chart")
  })

  outputEls <- split(outputEls, (1:(ncol*nrow) - 1) %/% ncol)
  rows <- lapply(outputEls, function(x) {
    do.call(shiny::fillRow, x)
  })

  do.call(shiny::fillCol, unname(rows))
}

.uiMenu <- function(ncharts, nrow, ncol, settingsBtn, okBtn, updateBtn) {
  container <- tags$div(
    class="mw-menu"
  )

  if (settingsBtn) {
    settingsBtn <- tags$div(
      class = "mw-btn mw-btn-settings",
      icon("gears"),
      tags$div(class="right-arrow")
    )
    container <- tagAppendChild(container, settingsBtn)
  }

  if (ncharts > 1) {
    container <- tagAppendChildren(container, list = .uiChartBtns(ncharts, nrow, ncol))
  }

  if (okBtn) {
    container <- tagAppendChild(container, tags$div(class = "mw-btn mw-btn-ok", "OK"))
  }
  container
}

.uiChartBtns <- function(ncharts, nrow, ncol) {
  lapply(seq_len(ncharts), function(i) {
    tags$div(
      class = "mw-btn mw-btn-area",
      .uiChartIcon(i, nrow, ncol),
      tags$div(class="right-arrow")
    )
  })
}

.uiChartIcon <- function(i, nrow, ncol) {
  WIDTH <- 40
  HEIGHT <- 30
  PAD <- 2
  i <- i - 1

  w <- (WIDTH - 2 * PAD) / ncol
  h <- (HEIGHT - 2 * PAD) / nrow

  chartIconStyle <- sprintf("width:%spx;height:%spx;left:%spx;top:%spx;",
                            w, h, w * (i%%ncol) + PAD, h * (i %/% nrow) + PAD)
  tags$div(
    class = "mw-icon-areachart",
    tags$div(class="mw-icon-chart", style=chartIconStyle)
  )
}
