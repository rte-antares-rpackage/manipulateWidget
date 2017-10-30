#Copyright © 2016 RTE Réseau de transport d’électricité

#' Add Controls to Interactive Plots
#'
#' @description
#' This function permits to add controls to an interactive plot created with
#' packages like \code{dygraphs}, \code{highcharter} or \code{plotly} in order
#' to change the input data or the parameters of the plot.
#'
#' Technically, the function starts a shiny gadget. The R session is bloqued
#' until the user clicks on "cancel" or "done". If he clicks on "done", then the
#' the function returns the last displayed plot so the user can modify it and/or
#' save it.
#'
#' @param .expr expression to evaluate that returns an interactive plot of class
#'   \code{htmlwidget}. This expression is re-evaluated each time a control is
#'   modified.
#' @param ... One or more named control arguments created with functions
#'   \code{\link{mwSlider}}, \code{\link{mwText}}, etc. The name of each control
#'   is the name of the variable the controls modifies in the expression. One
#'   can also create a group of inputs by passing a list of such control
#'   arguments. for instance \code{mygroup = list(txt = mwText(""), nb =
#'   mwNumeric(0))} creates a group of inputs named mygroup with two inputs
#'   named "txt" and "nb".
#' @param .updateBtn Should an update button be added to the controls ? If
#'   \code{TRUE}, then the graphic is updated only when the user clicks on the
#'   update button.
#' @param .saveBtn Should an save button be added to the controls ?
#' @param .updateBtnInit In case of update button. Do you want to render graphics on init ?
#' @param .viewer Controls where the gadget should be displayed. \code{"pane"}
#'   corresponds to the Rstudio viewer, \code{"window"} to a dialog window, and
#'   \code{"browser"} to an external web browser.
#' @param .compare Sometimes one wants to compare the same chart but with two
#'   different sets of parameters. This is the purpose of this argument. It can
#'   be a character vector of input names or a named list whose names are the
#'   names of the inputs that should vary between the two charts. Each element
#'   of the list must be a vector or a list of length equal to the number of
#'   charts with the initial values of the corresponding parameter for each
#'   chart. It can also be \code{NULL}. In this case, the parameter is
#'   initialized with the default value for the two charts.
#' @param .compareOpts List of options created \code{\link{compareOptions}}.
#'   These options indicate the number of charts to create and their disposition.
#' @param .return A function that can be used to modify the output of
#'   \code{manipulateWidget}. It must take two parameters: the first one is the
#'   final widget, the second one is a list of environments containing the input
#'   values of each individual widget. The length of this list is one if .compare
#'   is null, two or more if it has been defined.
#' @param .width Width of the UI. Used only on Rmarkdown documents with option
#'   \code{runtime: shiny}.
#' @param .height Height of the UI. Used only on Rmarkdown documents with option
#'   \code{runtime: shiny}.
#' @param .runApp (advanced usage) If true, a shiny gadget is started. If false,
#' the function returns a \code{\link{MWController}} object. This object can be
#' used to check with command line instructions the behavior of the application.
#' (See help page of \code{\link{MWController}}). Notice that this parameter is
#' always false in a non-interactive session (for instance when running tests of
#' a package).
#'
#'
#' @return
#' The result of the expression evaluated with the last values of the controls.
#' It should be an object of class \code{htmlWidget}.
#'
#' @section Advanced Usage:
#' The "normal" use of the function is to provide an expression that always
#' return an \code{htmlwidget}. In such case, every time the user changes the
#' value of an input, the current widget is destroyed and a new one is created
#' and rendered.
#'
#' Some packages provide functions to update a widget that has already been
#' rendered. This is the case for instance for package \code{leaflet} with the
#' function \code{\link[leaflet]{leafletProxy}}. To use such functions,
#' \code{manipulateWidget} evaluates the parameter \code{.expr} with four extra
#' variables:
#'
#' \itemize{
#'   \item{\code{.initial}:}{
#'     \code{TRUE} if the expression is evaluated for the first time and then
#'     the widget has not been rendered yet, \code{FALSE} if the widget has
#'     already been rendered.
#'   }
#'   \item{\code{.session}:}{
#'     A shiny session object.
#'   }
#'   \item{\code{.output}:}{
#'     ID of the output in the shiny interface.
#'   }
#'   \item{\code{.id}:}{
#'     Id of the chart. It can be used in comparison mode to make further
#'     customization without the need to create additional input controls.
#'   }
#' }
#'
#' You can take a look at the last example to see how to use these two
#' variables to update a leaflet widget.
#'
#' @section Modify the returned widget:
#'   In some specific situations, a developer may want to use
#'   \code{manipulateWidget} in a function that waits the user to click on the
#'   "Done" button and modifies the widget returned by \code{manipulateWidget}.
#'   In such situation, parameter \code{.return} should be used so that
#'   \code{manipulateWidget} is the last function called. Indeed, if other code
#'   is present after, the custom function will act very weird in a Rmarkdown
#'   document with "runtime: shiny".
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
#' # Comparison mode
#' if (require(dygraphs)) {
#'
#'   mydata <- data.frame(
#'     year = 2000+1:100,
#'     series1 = rnorm(100),
#'     series2 = rnorm(100),
#'     series3 = rnorm(100)
#'   )
#'
#'   manipulateWidget(
#'     dygraph(mydata[range[1]:range[2] - 2000, c("year", series)], main = title),
#'     range = mwSlider(2001, 2100, c(2001, 2100)),
#'     series = mwSelect(c("series1", "series2", "series3")),
#'     title = mwText("Fictive time series"),
#'     .compare = c("title", "series")
#'   )
#'
#'   # Setting different initial values for each chart
#'   manipulateWidget(
#'     dygraph(mydata[range[1]:range[2] - 2000, c("year", series)], main = title),
#'     range = mwSlider(2001, 2100, c(2001, 2100)),
#'     series = mwSelect(c("series1", "series2", "series3")),
#'     title = mwText(),
#'     .compare = list(
#'       title = list("First chart", "Second chart"),
#'       series = NULL
#'     )
#'   )
#' }
#'
#' # Grouping inputs
#' if (require(dygraphs)) {
#'
#'   mydata <- data.frame(year = 2000+1:100, value = rnorm(100))
#'   manipulateWidget(dygraph(mydata[range[1]:range[2] - 2000, ],
#'                            main = title, xlab = xlab, ylab = ylab),
#'                    range = mwSlider(2001, 2100, c(2001, 2100)),
#'                    "Graphical parameters" = mwGroup(
#'                       title = mwText("Fictive time series"),
#'                       xlab = mwText("X axis label"),
#'                       ylab = mwText("Y axis label")
#'                    )
#'                   )
#'
#' }
#'
#' # Example of conditional input controls
#' #
#' # In this example, we plot a x series against a y series. User can choose to
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
#'       plot_ly(dt, x= ~x, y = ~y, type = "scatter", mode = "markers")
#'     } else {
#'       plot_ly(dt, x= ~x, y = ~y, type = "scatter", mode = "lines", line = list(width = lwd))
#'     }
#'   }
#'
#'   manipulateWidget(
#'     myPlot(type, lwd),
#'     type = mwSelect(c("points", "lines"), "points"),
#'     lwd = mwSlider(1, 10, 1, .display = type == "lines")
#'   )
#'
#' }
#'
#' # Advanced Usage
#' #
#' # .expr is evaluated with extra variables .initial, .outputId and .session
#' # that can be used to update an already rendered widget instead of replacing
#' # it each time an input value is modified.
#' #
#' # Here we generate a UI that permits to change color and size of arbitrary
#' # points on a map generated with leaflet.
#'
#' if (require(leaflet)) {
#'   lon <- rnorm(10, sd = 20)
#'   lat <- rnorm(10, sd = 20)
#'
#'   myMapFun <- function(radius, color, initial, session, output) {
#'     if (initial) {
#'       # Widget has not been rendered
#'       map <- leaflet() %>% addTiles()
#'     } else {
#'       # widget has already been rendered
#'       map <- leafletProxy(output, session) %>% clearMarkers()
#'     }
#'
#'     map %>% addCircleMarkers(lon, lat, radius = radius, color = color)
#'   }
#'
#'   manipulateWidget(myMapFun(radius, color, .initial, .session, .output),
#'                    radius = mwSlider(5, 30, 10),
#'                    color = mwSelect(c("red", "blue", "green")))
#'
#' }
#'
#' @export
#'
manipulateWidget <- function(.expr, ..., .updateBtn = FALSE, .saveBtn = TRUE,
                             .updateBtnInit = FALSE,
                             .viewer = c("pane", "window", "browser"),
                             .compare = NULL,
                             .compareOpts = compareOptions(),
                             .return = function(widget, envs) {widget},
                             .width = NULL, .height = NULL, .runApp = TRUE) {

  # check if we are in runtime shiny
  isRuntimeShiny <- identical(knitr::opts_knit$get("rmarkdown.runtime"), "shiny")

  .expr <- substitute(.expr)
  .viewer <- match.arg(.viewer)
  .env <- parent.frame()
  .compareOpts <- do.call(compareOptions, .compareOpts)

  if (is.null(.compare)) {
      .compareOpts$ncharts <- 1
  } else {
    if (is.character(.compare)) {
      .compare <- sapply(.compare, function(x) NULL,
                         simplify = FALSE, USE.NAMES = TRUE)
    }

    if (is.null(.compareOpts$ncharts) || .compareOpts$ncharts < 2) {
      .compareOpts$ncharts <- 2
    }
  }

  dims <- .getRowAndCols(.compareOpts$ncharts, .compareOpts$nrow, .compareOpts$ncol)

  # Initialize inputs
  inputs <- initInputs(list(...), env = .env, compare = .compare,
                       ncharts = .compareOpts$ncharts)
  # Initialize controller
  controller <- MWController(.expr, inputs, autoUpdate = list(value = !.updateBtn, initBtn = .updateBtnInit),
                           nrow = dims$nrow, ncol = dims$ncol,
                           returnFunc = .return)

  if (.runApp & interactive()) {
    # We are in an interactive session so we start a shiny gadget
    .viewer <- switch(
      .viewer,
      pane = shiny::paneViewer(),
      window = shiny::dialogViewer("manipulateWidget"),
      browser = shiny::browserViewer()
    )

    ui <- mwModuleUI("ui", border = FALSE, okBtn = TRUE, saveBtn = .saveBtn,
                     width = "100%", height = "100%")
    server <- function(input, output, session) {
      mwModule("ui", controller)
    }

    shiny::runGadget(ui, server, viewer = .viewer)
  } else if (.runApp & isRuntimeShiny) {
    # We are in Rmarkdown document with shiny runtime. So we start a shiny app
    ui <- mwModuleUI("ui", margin = c("20px", 0), width = "100%", height = "100%")
    server <- function(input, output, session) {
      mwModule("ui", controller)
    }
    shiny::shinyApp(ui = ui, server = server, options = list(width = .width, height = .height))
  } else {
    # Other cases (Rmarkdown or non interactive execution). We return the controller
    # to not block the R execution.
    controller
  }
}
