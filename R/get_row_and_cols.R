# Copyright © 2016 RTE Réseau de transport d’électricité

# Private function that compute the "ideal" number of rows and columns given the
# number of widgets to display.
.getRowAndCols <- function(n, nrow = NULL, ncol = NULL) {
  if (!is.null(nrow) && !is.null(ncol) && nrow * ncol < n) {
    stop("There are too much widgets compared to the number of rows and columns")
  } else if (is.null(nrow) && !is.null(ncol)) {
    nrow <- ceiling(n / ncol)
  } else if (!is.null(nrow) && is.null(ncol)) {
    ncol <- ceiling(n / nrow)
  } else if (is.null(nrow) && is.null(ncol)) {
    nrow <- ceiling(sqrt(n))
    ncol <- ceiling(n / nrow)
  }

  list(nrow = nrow, ncol = ncol, n = n)
}
