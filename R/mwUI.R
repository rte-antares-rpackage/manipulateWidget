#' Private function that generates the general layout of the application
#'
#' @param controls Object returned by preprocessControls
#' @param ncol Number of columns in the chart area.
#' @param nrow Number of rows in the chart area.
#' @param outputFun Function that generates the html elements that will contain
#'   a given widget
#' @param okBtn Should the OK Button be added to the UI ?
#' @param updateBtn Should the updateBtn be added to the UI ? Currently unused.
#'
#' @return shiny tags
#'
#' @noRd
mwUI <- function(controls, nrow = 1, ncol = 1, outputFun = NULL,
                      okBtn = TRUE, updateBtn = FALSE) {

  htmldep <- htmltools::htmlDependency(
    "manipulateWidget",
    "0.7.0",
    system.file("manipulate_widget", package = "manipulateWidget"),
    script = "manipulate_widget.js",
    style = "manipulate_widget.css"
  )

  showSettings <- controls$nmod == 1 || length(controls$controls$shared) > 0

  container <- fillPage(
    tags$div(
      class="mw-container",
      fillRow(
        flex = c(NA, NA, 1),
        .uiMenu(controls$nmod, nrow, ncol, showSettings, okBtn, updateBtn),
        .uiControls(controls),
        .uiChartarea(controls$nmod, nrow, ncol, outputFun)
      )
    )
  )

  htmltools::attachDependencies(container, htmldep, TRUE)
}

.uiControls <- function(controls) {
   controls <- c(list(controls$controls$shared), controls$controls$ind)
   controls <- unname(lapply(controls, function(x) {
     if (length(x) == 0) return(NULL)
     tags$div(class = "mw-inputs", mwControlsUI(x))
   }))

   controls$class <- "mw-input-container"
   do.call(tags$div, controls)
}

.uiChartarea <- function(ncharts, nrow, ncol, outputFun) {
  outputEls <- lapply(seq_len(nrow * ncol), function(i) {
    if (i > ncharts) return(tags$div())
    outputId <- paste0("output", i)
    if (is.null(outputFun)) {
      el <- shiny::htmlOutput(outputId, style="width:100%;height:100%;")
    } else {
      el <- outputFun(outputId, width = "100%", height = "100%")
    }
    tags$div(class="mw-chart", el)
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
      tags$div(
        class = "bt1",
        icon("gears")
      ),
      tags$div(class="right-arrow")
    )
    container <- tagAppendChild(container, settingsBtn)
  }

  if (ncharts > 1) {
    container <- tagAppendChild(container, .uiChartBtns(ncharts, nrow, ncol))
  }

  if (updateBtn) {
    updateBtn <- tags$div(
      class = "mw-btn mw-btn-update",
      shiny::actionButton(".update", "", icon = shiny::icon("refresh"), class = "bt1")
    )
    container <- tagAppendChild(container, updateBtn)
  }

  if (okBtn) {
    okBtn <- shiny::actionButton("done", "OK", class = "mw-btn mw-btn-ok")
    container <- tagAppendChild(container, okBtn)
  }
  container
}

.uiChartBtns <- function(ncharts, nrow, ncol) {
  btns <- lapply(seq_len(ncharts), function(i) {
    tags$div(
      class = "mw-btn mw-btn-area",
      .uiChartIcon(i, nrow, ncol),
      tags$div(class="right-arrow")
    )
  })

  btns$class <- "mw-chart-selection"

  do.call(tags$div, btns)
}

.uiChartIcon <- function(i, nrow, ncol) {
  WIDTH <- 42
  HEIGHT <- 28
  PAD <- 2
  i <- i - 1

  w <- (WIDTH - 2 * PAD) / ncol
  h <- (HEIGHT - 2 * PAD) / nrow

  chartIconStyle <- sprintf("width:%spx;height:%spx;left:%spx;top:%spx;",
                            w, h, w * (i%%ncol) + PAD, h * (i %/% ncol) + PAD)
  tags$div(
    class = "mw-icon-areachart",
    tags$div(class="mw-icon-chart", style=chartIconStyle)
  )
}
