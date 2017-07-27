#' Private function that generates the general layout of the application
#'
#' @param inputs Object returned by preprocessInputs
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
mwUI <- function(inputs, nrow = 1, ncol = 1, outputFun = NULL,
                 okBtn = TRUE, updateBtn = FALSE, areaBtns = TRUE, border = FALSE) {

  htmldep <- htmltools::htmlDependency(
    "manipulateWidget",
    "0.7.0",
    system.file("manipulate_widget", package = "manipulateWidget"),
    script = "manipulate_widget.js",
    style = "manipulate_widget.css"
  )

  showSettings <- inputs$ncharts == 1 || length(inputs$inputs$shared) > 0
  if (border) class <- "mw-container with-border"
  else class <- "mw-container"

  container <- fillPage(
    tags$div(
      class = class,
      fillRow(
        flex = c(NA, NA, 1),
        .uiMenu(inputs$ncharts, nrow, ncol, showSettings, okBtn, updateBtn, areaBtns),
        .uiInputs(inputs),
        .uiChartarea(inputs$ncharts, nrow, ncol, outputFun)
      )
    )
  )

  htmltools::attachDependencies(container, htmldep, TRUE)
}

.uiInputs <- function(inputs) {
   inputs <- c(list(inputs$inputs$shared), inputs$inputs$ind)
   inputs <- unname(lapply(inputs, function(x) {
     if (length(x) == 0) return(NULL)
     content <- lapply(x, function(i) i$getHTML())
     tags$div(class = "mw-inputs", shiny::tagList(content))
   }))

   inputs$class <- "mw-input-container"
   do.call(tags$div, inputs)
}

.uiChartarea <- function(ncharts, nrow, ncol, outputFun) {
  outputEls <- lapply(seq_len(nrow * ncol), function(i) {
    if (i > ncharts) return(tags$div())
    outputId <- paste0("output_", i)
    if (is.null(outputFun)) {
      el <- combineWidgetsOutput(outputId, width = "100%", height = "100%")
    } else {
      el <- outputFun(outputId, width = "100%", height = "100%")
    }
    style <- sprintf("float:left;width:%s%%;height:%s%%",
                     floor(100 / ncol), floor(100 / nrow))
    tags$div(class="mw-chart", el, style = style)
  })

  tags$div(
    style = "height:100%;width:100%",
    shiny::tagList(outputEls)
  )
}

.uiMenu <- function(ncharts, nrow, ncol, settingsBtn, okBtn, updateBtn, areaBtns) {
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

  if (areaBtns && ncharts > 1) {
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
