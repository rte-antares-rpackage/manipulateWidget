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
#' @param .saveBtn Should an save button be added to the controls ? For saving output as html. Does not work in RStudio Viewer
#' @param .exportBtn Should an export button be added to the controls ? For saving output as png. Does not work in RStudio Viewer
#' @param .exportType \code{.exportBtn}, using \code{html2canvas} (default) and keeping current zoom, ... or using \code{webshot}
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
#' @param .translations List of translation strings created with function
#'   \code{\link{mwTranslations}}. Used to translate UI titles and labels.
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
#' @example inst/examples/manipulate_widget.R
#' @export
#'
manipulateWidget <- function(.expr, ..., .updateBtn = FALSE, .saveBtn = TRUE,
                             .exportBtn = TRUE, .exportType = c("html2canvas", "webshot"),
                             .viewer = c("pane", "window", "browser"),
                             .compare = NULL,
                             .compareOpts = compareOptions(),
                             .translations = mwTranslations(),
                             .return = function(widget, envs) {widget},
                             .width = NULL, .height = NULL, .runApp = TRUE) {

  # check if we are in runtime shiny
  isRuntimeShiny <- identical(knitr::opts_knit$get("rmarkdown.runtime"), "shiny")

  .expr <- substitute(.expr)
  .viewer <- match.arg(.viewer)
  .env <- parent.frame()
  .compareOpts <- do.call(compareOptions, .compareOpts)
  .translations <- do.call(mwTranslations, .translations)

  .exportType <- match.arg(.exportType)
  if (.exportType == "webshot" && !requireNamespace("webshot")) {
    stop("Package 'webshot' has not been installed. Install it or use argument .exportType = 'html2canvas'")
  }

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
  l_inputs <- list(...)
  if(".updateBtnInit" %in% names(l_inputs)){
    warning(".updateBtnInit is deprecated. Graphics are now always render on init")
    l_inputs$.updateBtnInit <- NULL
  }
  inputs <- initInputEnv(l_inputs, env = .env, compare = .compare,
                       ncharts = .compareOpts$ncharts)
  # Initialize controller
  controller <- MWController(.expr, inputs,
                             autoUpdate = list(value = !.updateBtn, saveBtn = .saveBtn,
                                               exportBtn = .exportBtn, exportType = .exportType),
                             nrow = dims$nrow, ncol = .compareOpts$ncol,
                             returnFunc = .return, translations = .translations)

  if (.runApp & interactive()) {
    # We are in an interactive session so we start a shiny gadget
    .viewer <- switch(
      .viewer,
      pane = shiny::paneViewer(),
      window = shiny::dialogViewer("manipulateWidget"),
      browser = shiny::browserViewer()
    )

    ui <- mwUI("mw", border = FALSE, okBtn = TRUE, updateBtn = .updateBtn,
                     saveBtn = .saveBtn, exportBtn = .exportBtn, exportType = .exportType,
                     width = "100%", height = "100%", allowCompare = .compareOpts$allowCompare)

    server <- function(input, output, session) {
      callModule(mwModuleServer, "mw", controller)
    }

    shiny::runGadget(ui, server, viewer = .viewer)
  } else if (.runApp & isRuntimeShiny) {
    # We are in Rmarkdown document with shiny runtime. So we start a shiny app
    ui <- mwUI("mw", border = TRUE, okBtn = FALSE, updateBtn = .updateBtn,
               saveBtn = .saveBtn, exportBtn = .exportBtn, exportType = .exportType,
               width = "100%", height = "100%", allowCompare = .compareOpts$allowCompare)
    server <- function(input, output, session) {
      callModule(mwModuleServer, "mw", controller)
    }
    shiny::shinyApp(ui = ui, server = server, options = list(width = .width, height = .height))
  } else {
    # Other cases (Rmarkdown or non interactive execution). We return the controller
    # to not block the R execution.
    invisible(controller)
  }
}
