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
#' @param fillPage : \code{logical}. Render in a fillPage or not ? Defaut to FALSE
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
mwModule <- function(id, controller, fillPage = FALSE, ...) {
  shiny::callModule(mwModuleServer, id, ctrl = controller, ...)
}
