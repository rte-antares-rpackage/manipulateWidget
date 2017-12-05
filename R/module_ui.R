#' Add a manipulateWidget to a shiny application
#'
#' These two functions can be used to include a manipulateWidget object in a shiny application.
#' \code{mwModuleUI} must be used in the UI to generate the required HTML elements and add
#' javascript and css dependencies. \code{mwModule} must be called once in the server function
#' of the application.
#'
#' @param id A unique string that identifies the module
#' @param controller Object of class \code{\link{MWController}} returned by
#'   \code{\link{manipulateWidget}} when parameter \code{.runApp} is
#'   \code{FALSE}.
#' @param ... named arguments containing reactive values. They can be used to send data from
#'   the main shiny application to the module.
#'
#' @return \code{mwModuleUI} returns the required HTML elements for the module. mwModule is only
#' used for its side effects.
#'
#' @examples
#' if (interactive() & require("dygraphs")) {
#'   require("shiny")
#'   ui <- fillPage(
#'   fillRow(
#'     flex = c(NA, 1),
#'     div(
#'       textInput("title", label = "Title", value = "glop"),
#'       selectInput("series", "series", choices = c("series1", "series2", "series3"))
#'     ),
#'     mwModuleUI("ui", height = "100%")
#'   ))
#'
#'   server <- function(input, output, session) {
#'     mydata <- data.frame(
#'       year = 2000+1:100,
#'       series1 = rnorm(100),
#'       series2 = rnorm(100),
#'       series3 = rnorm(100)
#'     )
#'
#'     c <- manipulateWidget(
#'       {
#'         dygraph(mydata[range[1]:range[2] - 2000, c("year", series)], main = title)
#'       },
#'       range = mwSlider(2001, 2100, c(2001, 2050)),
#'       series = mwSharedValue(),
#'       title = mwSharedValue(), .runApp = FALSE,
#'       .compare = "range"
#'     )
#'     #
#'     mwModule("ui", c, title = reactive(input$title), series = reactive(input$series))
#'   }
#'
#'   shinyApp(ui, server)
#'
#'
#' }
#'
#' @export
mwModule <- function(id, controller, ...) {
  shiny::callModule(controller$getModuleServer(), id, ...)
}


#' @param border Should a border be added to the module?
#' @param okBtn Should the UI contain the OK button?
#' @param saveBtn Should the UI contain the save button?
#' @param margin Margin to apply around the module UI. Should be one two or four valid css
#'   units.
#' @param width Width of the module UI.
#' @param height Height of the module UI.
#' @param header	Tag or list of tags to display as a common header above all tabPanels.
#' @param footer	Tag or list of tags to display as a common footer below all tabPanels
#' @param fluidRow	Include module in a fluidRow ? Can be usefull in a shiny app. Defaut to FALSE
#'
#' @rdname mwModule
#' @export
mwModuleUI <- function(id, border = TRUE, okBtn = FALSE, saveBtn = TRUE, margin = 0,
                       width = "100%", height = 400, header = NULL, footer = NULL,
                       fluidRow = FALSE) {

  ns <- shiny::NS(id)
  for (i in seq_along(margin)) {
    margin[i] <- shiny::validateCssUnit(margin[i])
  }

  margin <-paste(margin, collapse = " ")

  class <- ""
  if (border) class <- c(class, "with-border")
  if(!okBtn) class <- c(class, "without-ok")
  if(!saveBtn) class <- c(class, "without-save")
  class <- paste(class, collapse = " ")

  if(fluidRow){
    res <- shiny::fluidRow(
      shiny::column(12,
                    header,
                    shiny::uiOutput(ns("ui"), container = function(...) {
                      tags$div(style=sprintf("width:%s;height:%s;padding:%s",
                                             shiny::validateCssUnit(width),
                                             shiny::validateCssUnit(height),
                                             margin),
                               class = class,
                               ...)
                    }),
                    footer
      )
    )
  } else {
    res <- shiny::tagList(
      header,
      shiny::uiOutput(ns("ui"), container = function(...) {
        tags$div(style=sprintf("width:%s;height:%s;padding:%s",
                               shiny::validateCssUnit(width),
                               shiny::validateCssUnit(height),
                               margin),
                 class = class,
                 ...)
      }),
      footer
    )
  }


  htmldep <- htmltools::htmlDependency(
    "manipulateWidget",
    "0.7.0",
    system.file("manipulate_widget", package = "manipulateWidget"),
    script = "manipulate_widget.js",
    style = "manipulate_widget.css"
  )

  htmltools::attachDependencies(res, htmldep, TRUE)
}
