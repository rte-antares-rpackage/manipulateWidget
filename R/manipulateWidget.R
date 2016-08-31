#' Add Controls to Interactive Plots
#'
#' @description
#' This function permits to add controls to an interactive plot created with
#' packages like \code{dygraphs}, \code{highcharter} or \code{plotly}in order
#' to change the input data or the parameters of the plot.
#'
#' Technically, the function starts a shiny gadget. The R session is bloqued
#' until the user clicks on "cancel" or "done". If he clicks on "done", then the
#' the function returns the last displayed plot so the user can modify it and/or
#' save it.
#'
#' @param .expr
#'   expression to evaluate that returns an interactive plot of class
#'   \code{htmlwidget}. This expression is re-evaluated each time a control is
#'   modified.
#' @param ...
#'   One or more named control arguments created with functions
#'   \code{\link{mwSlider}}, \code{\link{mwText}}, etc. The name of each control
#'   is the name of the variable the controls modifies in the expression.
#' @param .main
#'   Title of the shiny gadget
#' @param .updateBtn
#'   Should an update button be added to the controls ? If \code{TRUE}, then
#'   the graphic is updated only when the user clicks on the update button.
#' @param .controlPos
#'   Where controls should be placed ? By default, they are placed in the left,
#'   next to the graphic. If \code{controlPos = "tab"}, two tabs are created:
#'   one containing controls and the other containing the graphic.
#' @param .tabColumns
#'   If controls are placed in a distinct tab, how many columns should be
#'   used ? This parameter is used only if \code{controlPos = "tab"}
#' @param .viewer
#'   Controls where the gadget should be displayed. \code{"pane"} corresponds to
#'   the Rstudio viewer, \code{"window"} to a dialog window, and \code{"browser"}
#'   to an external web browser.
#' @param .display
#'   A named list of conditions that evaluate to TRUE OR FALSE indicating when
#'   inputs should be displayed. These conditions are reevaluated each time a
#'   control it modified. By default, each control is displayed, but if the name
#'   of a control appears in this list, then the associated condition is
#'   evaluated. If the result is TRUE then the control is visible, else it is
#'   hidden.
#'
#' @return
#' The result of the expression evaluated with the last values of the control.
#' It should be an object of class \code{htmlWidget}.
#'
#' @examples
#' if (require(dygraphs)) {
#'
#'   mydata <- data.frame(year = 2000+1:100, value = rnorm(100))
#'   manipulateWidget(dygraph(mydata[range[1]:range[2] - 2000, ], main = title),
#'                    range = mwSlider(2001, 2100, c(2001, 2100)),
#'                    title = mwText("Fictive time series"))
#'
#' }
#'
#' # Example of conditional input controls
#' #
#' # In this exemple, we plot a x series against a y series. User can choose to
#' # use points or lines. If he chooses lines, then an additional input is displayed
#' # to let him control the width of the lines.
#' if (require("plotly")) {
#'
#'   dt <- data.frame (
#'     x = sort(runif(100)),
#'     y = rnorm(100)
#'   )
#'
#'   myPlot <- function(type, lwd) {
#'     if (type == "points") {
#'       plot_ly(dt, x= x, y = y, mode = "markers")
#'     } else {
#'       plot_ly(dt, x= x, y = y, mode = "lines", line = list(width = lwd))
#'     }
#'   }
#'
#'   manipulateWidget(
#'     myPlot(type, lwd),
#'     type = mwSelect(c("points", "lines")),
#'     lwd = mwSlider(1, 10, 1),
#'     .display = list(lwd = type == "lines")
#'   )
#'
#' }
#'
#' @export
#'
manipulateWidget <- function(.expr, ..., .main = NULL, .updateBtn = FALSE,
                             .controlPos = c("left", "top", "right", "bottom", "tab"),
                             .tabColumns = 2,
                             .viewer = c("pane", "window", "browser"),
                             .display = NULL) {
  .expr <- substitute(.expr)
  .display <- substitute(.display)
  .viewer <- match.arg(.viewer)
  .env <- parent.frame()

  if (.controlPos == "tab") .updateBtn <- FALSE

  if (is.null(.main)) {
    .main <- paste(deparse(.expr), collapse = ";")
    .main <- gsub("^\\{ *;?", "", .main)
    .main <- gsub("\\}$", "", .main)
    if (nchar(.main) > 53) {
      .main <- substring(.main, 1, 50)
      .main <- paste0(.main, " ...")
    }
  }

  controlNames <- names(list(...))

  # Add an invisible checkbox for each control indicating if the control must be
  # displayed or not

  ui <- mwUI(
    ...,
    .controlPos = .controlPos,
    .tabColumns = .tabColumns,
    .updateBtn = .updateBtn,
    .main = .main
  )

  server <- function(input, output, session) {

    inputList <- reactive({
      input$.update

      res <- lapply(controlNames, function(s) {
        if (.updateBtn) eval(parse(text = sprintf("isolate(input$%s)", s)))
        else eval(parse(text = sprintf("input$%s", s)))
      })
      names(res) <- controlNames

      res
    })

    output$content <- renderUI({
      inputEnv <- list2env(inputList(), parent = .env)

      # Update the interface if parameter .display is set
      .displayBool <- eval(.display, envir = inputEnv)
      for (id in names(.displayBool)) {
        updateCheckboxInput(session, inputId = paste0(id, "_visible"),
                            value = .displayBool[[id]])
      }

      res <- .processOutput(eval(.expr, envir = inputEnv))
      fillCol(res)
    })

    observeEvent(input$done, {
      inputEnv <- list2env(inputList(), parent = .env)
      stopApp(eval(.expr, envir = inputEnv))
    })
  }

  .viewer <- switch(.viewer,
    pane = paneViewer(),
    window = dialogViewer(.main),
    browser = browserViewer()
  )

  runGadget(ui, server, viewer = .viewer)

}
