#' Private function that gets shiny output and render functions for a given htmlWidget
#'
#' @param x Object, generally a htmlwidget.
#'
#' @return A list with the following elements
#' - outputFunc
#' - renderFunc
#' - useCombineWidgets TRUE only if x is not an htmlwidget
#' @noRd
getOutputAndRenderFunc <- function(x) {
  # Get shiny output and render functions
  if (inherits(x, "htmlwidget")) {
    cl <- class(x)
    pkg <- attr(x, "package")

    renderFunName <- ls(getNamespace(pkg), pattern = "^render")
    renderFunction <- getFromNamespace(renderFunName, pkg)

    outputFunName <- ls(getNamespace(pkg), pattern = "Output$")
    outputFunction <- getFromNamespace(outputFunName, pkg)
    useCombineWidgets <- FALSE
  } else {
    renderFunction <- renderCombineWidgets
    outputFunction <- combineWidgetsOutput
    useCombineWidgets <- TRUE
  }

  list(
    outputFunc = outputFunction,
    renderFunc = renderFunction,
    useCombineWidgets = useCombineWidgets
  )
}
