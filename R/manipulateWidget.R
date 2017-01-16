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
#' @param .main Title of the shiny gadget
#' @param .updateBtn Should an update button be added to the controls ? If
#'   \code{TRUE}, then the graphic is updated only when the user clicks on the
#'   update button.
#' @param .controlPos Where controls should be placed ? By default, they are
#'   placed in the left, next to the graphic. If \code{controlPos = "tab"}, two
#'   tabs are created: one containing controls and the other containing the
#'   graphic.
#' @param .tabColumns If controls are placed in a distinct tab, how many columns
#'   should be used ? This parameter is used only if \code{controlPos = "tab"}
#' @param .viewer Controls where the gadget should be displayed. \code{"pane"}
#'   corresponds to the Rstudio viewer, \code{"window"} to a dialog window, and
#'   \code{"browser"} to an external web browser.
#' @param .display A named list of conditions that evaluate to TRUE OR FALSE
#'   indicating when inputs should be displayed. These conditions are
#'   reevaluated each time a control it modified. By default, each control is
#'   displayed, but if the name of a control appears in this list, then the
#'   associated condition is evaluated. If the result is TRUE then the control
#'   is visible, else it is hidden.
#' @param .updateInputs This parameter is similar to `.display` and can be used
#'  to dynamically update input controls. It must be a named list where names
#'  correspond to names of input controls and values are named lists of
#'  expressions where names correspond to arguments of the input generator
#'  function.
#' @param .compare Sometimes one wants to compare the same chart but with two
#'   different sets of parameters. This is the purpose of this argument. It must
#'   be a named list whose names are the names of the inputs that should vary
#'   between the two charts. All other parameters are common to the two charts
#'   and changing their values will change the two charts. Each element of the
#'   list must be a vector or a list of length 2 with the initial values of the
#'   corresponding parameter for each chart. It can also be \code{NULL}. In this
#'   case, the parameter is initialized with the default value for the two
#'   charts.
#' @param .compareLayout Used only when \code{.compare} is set. Possible values
#'   are "v" for vertical layout (one chart above the other) and "h" for
#'   horizontal layout (one chart on the right of the other)
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
#'   manipulateWidget(
#'     dygraph(mydata[range[1]:range[2] - 2000, c("year", series)], main = title),
#'     range = mwSlider(2001, 2100, c(2001, 2100)),
#'     series = mwSelect(c("series1", "series2", "series3")),
#'     title = mwText("Fictive time series"),
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
#'                    "Graphical parameters" = list(
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
#'     lwd = mwSlider(1, 10, 1),
#'     .display = list(lwd = type == "lines")
#'   )
#'
#' }
#'
#' # Advanced Usage
#' #
#' # .expr is evaluated with two extra variables .initial and .session that can
#' # be used to update an already rendered widget instead of replacing it each
#' # time an input value is modified.
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
manipulateWidget <- function(.expr, ..., .main = NULL, .updateBtn = FALSE,
                             .controlPos = c("left", "top", "right", "bottom", "tab"),
                             .tabColumns = 2,
                             .viewer = c("pane", "window", "browser"),
                             .display = NULL,
                             .updateInputs = NULL,
                             .compare = NULL,
                             .compareLayout = c("v", "h")) {

  # check if we are in runtime shiny
  isRuntimeShiny <- identical(knitr::opts_knit$get("rmarkdown.runtime"), "shiny")

  .expr <- substitute(.expr)
  .display <- substitute(.display)
  .updateInputs <- substitute(.updateInputs)
  .viewer <- match.arg(.viewer)
  .controlPos <- match.arg(.controlPos)
  .compareLayout <- match.arg(.compareLayout)
  .env <- parent.frame()
  compareMode <- !is.null(.compare)

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

  # Evaluate a first time .expr to determine the class of the output
  controls <- comparisonControls(list(...), .compare, .updateInputs)
  controlDesc <- getControlDesc(controls[c("common", "ind")])

  initValues <- controlDesc$initValue
  names(initValues) <- controlDesc$name

  selectInputList <- controlDesc$name[controlDesc$type == "select"]

  # Add a parameter indicating this is the first evaluation of .expr
  initValues$.initial <- TRUE
  initValues$.session <- NULL
  initValues$.output <- "output"
  initValues$.id <- 1

  initWidget <- eval(.expr, envir = list2env(initValues, parent = .env))

  if (compareMode) {
    controlDesc2 <- getControlDesc(controls[c("common", "ind2")])
    initValues2 <- controlDesc2$initValue
    names(initValues2) <- controlDesc$name
    initValues2$.initial <- TRUE
    initValues2$.session <- NULL
    initValues2$.output <- "output2"
    initValues2$.id <- 2

    for (v in names(.compare)) {
      if (!is.null(.compare[[v]])) {
        initValues[[v]] <- .compare[[v]][[1]]
        initValues2[[v]] <- .compare[[v]][[2]]
      }
    }

    initWidget2 <- eval(.expr, envir = list2env(initValues2, parent = .env))
  } else {
    initWidget2 = NULL
    initValues2 <- NULL
  }

  # Get shiny output and render functions
  if (is(initWidget, "htmlwidget")) {
    cl <- class(initWidget)[1]
    pkg <- attr(initWidget, "package")

    renderFunName <- ls(getNamespace(pkg), pattern = "^render")
    renderFunction <- getFromNamespace(renderFunName, pkg)

    OutputFunName <- ls(getNamespace(pkg), pattern = "Output$")
    outputFunction <- getFromNamespace(OutputFunName, pkg)
  } else {
    renderFunction <- renderUI
    outputFunction <- NULL
  }

  # UI
  ui <- mwUI(
    ...,
    .controlPos = .controlPos,
    .tabColumns = .tabColumns,
    .updateBtn = .updateBtn,
    .main = .main,
    .outputFun = outputFunction,
    .titleBar = !isRuntimeShiny,
    .updateInputs = .updateInputs,
    .compare = .compare,
    .compareLayout = .compareLayout
  )

  server <- mwServer(.expr, initWidget, initWidget2,
                     initValues, initValues2,
                     renderFunction,
                     controlDesc, .display, .updateInputs,
                     .compare, .compareLayout,
                     .updateBtn, .env)

  if (interactive()) {
    # We are in an interactive session so we start a shiny gadget
    .viewer <- switch(.viewer,
      pane = paneViewer(),
      window = dialogViewer(.main),
      browser = browserViewer()
    )
    runGadget(ui, server, viewer = .viewer)
  } else if (isRuntimeShiny) {
    # We are in Rmarkdown document with shiny runtime. So we start a shiny app
    shinyApp(ui = ui, server = server)
  } else {
    # Other cases (Rmarkdown or non interactive execution). We return the initial
    # widget to not block the R execution.
    initWidget
  }
}
