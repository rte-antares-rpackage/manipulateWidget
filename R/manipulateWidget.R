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
#' @export
#'
manipulateWidget <- function(.expr, ..., .main = NULL, .updateBtn = FALSE,
                             .controlPos = c("left", "top", "right", "bottom", "tab"),
                             .tabColumns = 2) {
  .expr <- substitute(.expr)
  .controlPos <- match.arg(.controlPos)

  if (.controlPos == "tab") .updateBtn <- FALSE

  if (is.null(.main)) {
    .main <- paste(deparse(.expr), collapse = " ")
    if (nchar(.main) > 53) {
      .main <- substring(.main, 1, 50)
      .main <- paste0(.main, " ...")
    }
  }

  controls <- list(...)
  controlNames <- names(controls)

  controls <- mapply(function(f, id) f(id), f = controls, id = controlNames,
                     SIMPLIFY = FALSE, USE.NAMES = FALSE)

  if (.updateBtn) controls <- append(controls, list(actionButton(".update", "Update",
                                                                 class = "btn-primary")))

  ui <- miniPage(
    gadgetTitleBar(.main),
    .ui(controls, .controlPos, .tabColumns)
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
      res <- .processOutput(eval(.expr, envir = inputList()))
      fillCol(res)
    })

    observeEvent(input$done, {
      stopApp(.processOutput(eval(.expr, envir = inputList())))
    })
  }

  runGadget(ui, server)

}
