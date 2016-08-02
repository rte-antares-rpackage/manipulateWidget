#' Combine several interactive plots
#'
#' @export
#'
combineWidgets <- function(..., nrow = NULL, ncol = NULL, hflex = 1, vflex = 1) {
  widgets <- lapply(list(...), function(x) {
    x$width <- x$height <- "100%"
    x
  })

  # Get Number of rows and columns
  nwidgets <- length(widgets)
  if (!is.null(nrow) && !is.null(ncol) && nrow * ncol < nwidgets) {
    stop("There are too much widgets compared to the number of rows and columns")
  } else if (is.null(nrow) && !is.null(ncol)) {
    nrow <- ceiling(nwidgets / ncol)
  } else if (!is.null(nrow) && is.null(ncol)) {
    ncol <- ceiling(nwidgets / nrow)
  } else {
    nrow <- ceiling(sqrt(nwidgets))
    ncol <- ceiling(nwidgets / nrow)
  }

  rows <- lapply(1:nrow, function(i) {
    args <- widgets[((i-1) * ncol + 1):(i * ncol)]
    args$flex <- hflex
    do.call(fillRow, args)
  })

  rows$flex <- vflex

  res <- do.call(fillCol, rows)
  class(res) <- append("combinedHtmlwidgets", class(res))
  res
}

print.combinedHtmlwidgets <- function(x, ...) {
  htmltools:::html_print(miniPage(x))
}
