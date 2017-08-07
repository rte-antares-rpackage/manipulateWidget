#' Private function that generates the general layout of the application
#'
#' @param ns namespace function created with shiny::NS(). Useful to create
#'   modules.
#' @param inputs Object returned by preprocessInputs
#' @param ncol Number of columns in the chart area.
#' @param nrow Number of rows in the chart area.
#' @param outputFun Function that generates the html elements that will contain
#'   a given widget
#' @param okBtn Should the OK Button be added to the UI ?
#' @param saveBtn Should the Save Button be added to the UI ?
#' @param updateBtn Should the updateBtn be added to the UI ? Currently unused.
#' @param width, height	Must be a valid CSS unit (like "100%", "400px", "auto") or a number,
#' which will be coerced to a string and have "px" appended. Default to "100%" & "400px"
#'
#' @return shiny tags
#'
#' @noRd
mwUI <- function(ns, inputs, nrow = 1, ncol = 1, outputFun = NULL,
                 okBtn = TRUE, saveBtn = TRUE, updateBtn = FALSE,
                 areaBtns = TRUE, border = FALSE,
                 width = "100%", height = "400px") {

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
      style = paste("width:", width, ";height:", height, ";"),
      fillRow(
        flex = c(NA, NA, 1),
        .uiMenu(ns, inputs$ncharts, nrow, ncol, showSettings, okBtn, saveBtn, updateBtn, areaBtns),
        .uiInputs(ns, inputs),
        .uiChartarea(ns, inputs$ncharts, nrow, ncol, outputFun)
      )
    )
  )

  htmltools::attachDependencies(container, htmldep, TRUE)
}

.uiInputs <- function(ns, inputs) {
   inputs <- c(list(inputs$inputs$shared), inputs$inputs$ind)
   ids <- ns(c("mw-shared-inputs", paste0("mw-ind-inputs-", 1:(length(inputs) - 1))))
   inputs <- mapply(function(x, id) {
     if (length(x) == 0) return(NULL)
     content <- lapply(x, function(i) i$getHTML(ns))
     tags$div(class = "mw-inputs", id = id, shiny::tagList(content))
   }, x = inputs, id = ids, USE.NAMES = FALSE, SIMPLIFY = FALSE)

   inputs$class <- "mw-input-container"
   do.call(tags$div, inputs)
}

.uiChartarea <- function(ns, ncharts, nrow, ncol, outputFun) {
  outputEls <- lapply(seq_len(nrow * ncol), function(i) {
    if (i > ncharts) return(tags$div())
    outputId <- ns(paste0("output_", i))
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

.uiMenu <- function(ns, ncharts, nrow, ncol, settingsBtn, okBtn, saveBtn, updateBtn, areaBtns) {
  container <- tags$div(
    class="mw-menu"
  )

  if (settingsBtn) {
    settingsBtn <- tags$div(
      class = "mw-btn mw-btn-settings",
      onclick = sprintf("select(this, '%s')", ns("mw-shared-inputs")),
      tags$div(
        class = "bt1",
        icon("gears")
      ),
      tags$div(class="right-arrow")
    )
    container <- tagAppendChild(container, settingsBtn)
  }

  if (areaBtns && ncharts > 1) {
    container <- tagAppendChild(container, .uiChartBtns(ns, ncharts, nrow, ncol))
  }

  if (updateBtn) {
    updateBtn <- tags$div(
      class = "mw-btn mw-btn-update",
      shiny::actionButton(ns(".update"), "", icon = shiny::icon("refresh"), class = "bt1")
    )
    container <- tagAppendChild(container, updateBtn)
  }

  if (okBtn) {
    okBtnInput <- shiny::actionButton(ns("done"), "OK", class = "mw-btn mw-btn-ok")
    container <- tagAppendChild(container, okBtnInput)
  }

  if (saveBtn) {
    bottom_px <- ifelse(okBtn, "bottom: 80px;", "bottom: 30px;")
    saveBtnInput <- shiny::downloadButton(ns("save"), label = "", class = "mw-btn mw-btn-save",
                                     style = bottom_px)
    container <- tagAppendChild(container, saveBtnInput)
  }

  container
}

.uiChartBtns <- function(ns, ncharts, nrow, ncol) {
  ids <- ns(paste0("mw-ind-inputs-", seq_len(ncharts)))
  btns <- lapply(seq_len(ncharts), function(i) {
    tags$div(
      class = "mw-btn mw-btn-area",
      onclick = sprintf("select(this,'%s')", ids[i]),
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
