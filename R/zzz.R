# Copyright © 2016 RTE Réseau de transport d’électricité

#' @name manipulateWidget-package
#'
#' @title Add even more interactivity to interactive charts
#'
#' @description
#' This package is largely inspired by the \code{manipulate} package from
#' Rstudio. It can be used to easily create graphical interface that let the
#' user modify the data or the parameters of an interactive chart. It also
#' provides the \code{\link{combineWidgets}} function to easily combine multiple
#' interactive charts in a single view.
#'
#' @details
#' \code{\link{manipulateWidget}} is the main function of the package. It
#' accepts an expression that generates an interactive chart (and more precisely
#' an \code{htmlwidget} object. See \url{http://www.htmlwidgets.org/} if you
#' have never heard about it) and a set of controls created with functions
#' \code{mwSlider}, \code{mwCheckbox}... which are used to dynamically change
#' values within the expression. Each time the user modifies the value of a
#' control, the expression is evaluated again and the chart is updated. Consider
#' the following code:
#'
#' \code{manipulateWidget(myPlotFun(country), country = mwSelect(c("BE", "DE", "ES", "FR")))}
#'
#' It will generate a graphical interface with a select input on its left with
#' options "BE", "DE", "ES", "FR". By default, at the beginning the value of the
#' variable \code{country} will be equal to the first choice of the
#' corresponding input. So the function will first execute
#' \code{myPlotFun("BE")} and the result will be displayed in the main panel of
#' the interface. If the user changes the value to "FR", then the expression
#' \code{myPlotFun("FR")} is evaluated and the new result is displayed.
#'
#' The interface also contains a button "Done". When the user clicks on it, the
#' last chart is returned. It can be stored in a variable, be modified by the
#' user, saved as a html file with \code{\link[htmlwidgets]{saveWidget}} from package
#' \code{htmlwidgets} or converted to a static image file with package
#' \code{webshot}.
#'
#' Finally one can easily create complex layouts thanks to function
#' \code{\link{combineWidgets}}. For instance, assume we want to see a map that
#' displays values of some variable for a given year, but on its right side we also
#' want to see the distributions of three variables. Then we could write:
#'
#' \preformatted{
#' myPlotFun <- function(year, variable) {
#'   combineWidgets(
#'     ncol = 2, colSize = c(3, 1),
#'     myMap(year, variable),
#'     combineWidgets(
#'       ncol = 1,
#'       myHist(year, "V1"),
#'       myHist(year, "V2"),
#'       myHist(year, "V3"),
#'     )
#'   )
#' }
#'
#' manipulateWidget(
#'   myPlotFun(year, variable),
#'   year = mwSlider(2000, 2016, value = 2000),
#'   variable = mwSelect(c("V1", "V2", "V3"))
#' )
#' }
#'
#' Of course, \code{\link{combineWidgets}} can be used outside of
#' \code{\link{manipulateWidget}}. For instance, it can be used in an
#' Rmarkdown document to easily put together interactive charts.
#'
#' For more concrete examples of usage, you should look at the documentation and
#' especially the examples of \code{\link{manipulateWidget}} and
#' \code{\link{combineWidgets}}.
#'
#' @seealso \code{\link{manipulateWidget}}, \code{\link{combineWidgets}}
#'
#' @rdname manipulateWidget-package
#' @docType package
#' @importFrom shiny tags observe observeEvent reactive isolate icon tagAppendChild
#' @importFrom shiny tagAppendChildren fillPage fillRow
#' @importFrom miniUI miniContentPanel miniPage miniTabPanel miniTabstripPanel gadgetTitleBar
#' @importFrom htmlwidgets getDependency
#' @importFrom methods is new setRefClass
#' @importFrom utils getFromNamespace
#' @importFrom stats runif
NULL
#
globalVariables(c("mod", "multiple", "name", "type"))
