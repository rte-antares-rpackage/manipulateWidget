#' Options for comparison mode
#'
#' This function generates a list of options that are used by
#' \code{\link{manipulateWidget}} to compare multiple charts.
#'
#' @param ncharts Number of charts to generate.
#' @param nrow Number of rows. If \code{NULL}, the function tries to pick the
#'   best number of rows given the number of charts and columns.
#' @param ncol Number of columns. If \code{NULL}, the function tries to pick the
#'   best number of columns given the number of charts and rows.
#'
#' @return List of options
#'
#' @examples
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
#'     .compare = list(title = NULL, series = NULL),
#'     .compareOpts = compareOptions(ncharts = 4)
#'   )
#'
#'   manipulateWidget(
#'     dygraph(mydata[range[1]:range[2] - 2000, c("year", series)], main = title),
#'     range = mwSlider(2001, 2100, c(2001, 2100)),
#'     series = mwSelect(c("series1", "series2", "series3")),
#'     title = mwText("Fictive time series"),
#'     .compare = list(title = NULL, series = NULL),
#'     .compareOpts = compareOptions(ncharts = 3, nrow = 3)
#'   )
#' }
#'
#' @export
compareOptions <- function(ncharts = NULL, nrow = NULL, ncol = NULL) {
  list(
    ncharts = ncharts,
    nrow = nrow,
    ncol = ncol
  )
}
