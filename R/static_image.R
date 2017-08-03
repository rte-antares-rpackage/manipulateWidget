#' Include a static image in a combinedWidgets
#'
#' \code{staticPlot} is a function that generates a static plot and then return
#' the HTML code needed to include the plot in a combinedWidgets.
#' \code{staticImage} is a more general function that generates the HTML code
#' necessary to include any image file.
#'
#' @param expr Expression that creates a static plot.
#' @param width Width of the image to create.
#' @param height Height of the image to create.
#' @param file path of the image to include.
#' @param style CSS style to apply to the image.
#'
#' @return a \code{shiny.tag} object containing the HTML code required to include
#' the image or the plot in a \code{combinedWidgets} object.
#'
#' @examples
#' staticPlot(hist(rnorm(100)))
#'
#' if (require(plotly)) {
#'   data(iris)
#'
#'   combineWidgets(
#'     plot_ly(iris, x = ~Sepal.Length, type = "histogram", nbinsx = 20),
#'     staticPlot(hist(iris$Sepal.Length, breaks = 20), height = 300)
#'   )
#'
#'   # You can also embed static images in the header, footer, left or right
#'   # columns of a combinedWidgets. The advantage is that the space allocated
#'   # to the static plot will be constant when the window is resized.
#'
#'   combineWidgets(
#'     plot_ly(iris, x = ~Sepal.Length, type = "histogram", nbinsx = 20),
#'     footer = staticPlot(hist(iris$Sepal.Length, breaks = 20), height = 300)
#'   )
#' }
#'
#' @importFrom grDevices dev.off png
#' @export
staticPlot <- function(expr, width = 600, height = 400) {
  expr <- substitute(expr)
  file <- tempfile(fileext = ".png")
  png(file, width, height)
  eval(expr)
  dev.off()
  staticImage(file)
}

#' @rdname staticPlot
#' @export
#'
staticImage <- function(file, style = "max-width:100%%;max-height:100%%") {
  data <- base64enc::base64encode(readBin(file, "raw", file.info(file)[1, "size"]))
  ext <- tools::file_ext(file)
  tags$img(
    src = sprintf("data:image/%s;base64,%s", ext, data),
    style = style
  )
}
