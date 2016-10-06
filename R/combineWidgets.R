#Copyright © 2016 RTE Réseau de transport d’électricité

#' Combine several interactive plots
#'
#' This function combines different interactive plots in a unique view. It is
#' especially useful in the function \code{\link{manipulateWidget}} to have in
#' the same window several related plots that respond to the same set of
#' controls.
#'
#' @param ...
#'   Elements to combine. They should be htmlwidgets, but they can also be
#'   shiny tags or html object or text
#' @param nrow
#'   Number of rows of the layout.
#' @param ncol
#'   Number of columns of the layout.
#' @param title
#'   Title of the view
#' @param hflex
#'   This argument controls the relative size of each column. For instance, if
#'   the layout has two columns and \code{hflex = c(2,1)}, then the width of the
#'   first column will be twice the one of the second one. If a value is equal
#'   to NA, then the corresponding column will have its 'natural' width, and the
#'   remaining space will be shared between the other columns.
#' @param vflex
#'   Same as hflex but for the height of the rows of the layout.
#'
#' @return
#' Object of class 'combinedHtmlwidgets' which is an extension of 'siny.tags'.
#'
#' @details
#' The function only allows table like layout : each row has the same number of
#' columns and reciprocally. But it is possible to create more complex layout
#' by nesting combined htmlwidgets. (see examples)
#'
#' @examples
#' if require(plotly) {
#'   data(iris)
#'
#'  combineWidgets(title = "The Iris dataset",
#'    plot_ly(iris, x = Sepal.Length, type = "histogram", nbinsx = 20),
#'    plot_ly(iris, x = Sepal.Width, type = "histogram", nbinsx = 20),
#'    plot_ly(iris, x = Petal.Length, type = "histogram", nbinsx = 20),
#'    plot_ly(iris, x = Petal.Width, type = "histogram", nbinsx = 20)
#'  )
#'
#'  # Create a more complex layout by nesting combinedWidgets
#'  combineWidgets(title = "The iris data set: sepals", ncol = 2, hflex = c(2,1),
#'    plot_ly(iris, x = Sepal.Length, y = Sepal.Width, mode = "markers", color = Species),
#'    combineWidgets(
#'      plot_ly(iris, x = Sepal.Length, type = "histogram", nbinsx = 20),
#'      plot_ly(iris, x = Sepal.Width, type = "histogram", nbinsx = 20)
#'    )
#'  )
#'
#'  # Use combineWidgets with manipulateWidget
#' manipulateWidget({
#'   if (length(species) == 0) return ("Please choose a species")
#'
#'   data <- subset(iris, Species %in% species)
#'
#'   colors <- c("#A020F0", "#FFA500", "#2020FF")
#'   names(colors) <- levels(iris$Species)
#'   colors <- colors[species]
#'
#'   combineWidgets(title = "The iris data set: sepals", ncol = 2, hflex = c(2,1),
#'     plot_ly(data, x = Sepal.Length, y = Sepal.Width, mode = "markers",
#'             color = droplevels(Species), colors = colors),
#'     combineWidgets(
#'       plot_ly(data, x = Sepal.Length, type = "histogram", nbinsx = 20),
#'       plot_ly(data, x = Sepal.Width, type = "histogram", nbinsx = 20)
#'     )
#'   )
#' },
#' species = mwCheckboxGroup(levels(iris$Species)))
#' }
#'
#' @export
#'
combineWidgets <- function(..., nrow = NULL, ncol = NULL, title = NULL,
                           hflex = 1, vflex = 1) {

  widgets <- lapply(list(...), .processOutput)

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

  hflex <- rep(hflex, length.out = ncol)
  vflex <- rep(vflex, length.out = nrow)

  rows <- lapply(1:nrow, function(i) {
    args <- widgets[((i-1) * ncol + 1):(i * ncol)]

    # If vflex is NA for this row, then try to infer the height of the row.
    if (is.na(vflex[i])) {
      heights <- unlist(sapply(args, function(x) {
        if (!is.list(x)) return (NULL)
        if (!is.null(x$height)) return(x$height)
        if (!is.null(x$attribs)) return(x$attribs$height)
        NULL
      }))
      if (!is.null(heights)) {
        heights <- na.omit(heights)
        if (length(heights) > 0)  args$height <- heights[1]
      }
      if (is.null(args$height)) args$height <- 200
    }

    args$flex <- hflex
    do.call(fillRow, args)
  })

  # Title
  if(!is.null(title)) {
    vflex <- c(NA, vflex)
    title <- tags$div(style = "text-align: center;",
      tags$h1(title)
    )
    rows <- append(list(title), rows)
  }

  rows$flex <- vflex

  res <- do.call(fillCol, rows)
  class(res) <- append("combinedHtmlwidgets", class(res))
  res
}

#' @export
print.combinedHtmlwidgets <- function(x, ...) {
  htmltools:::html_print(miniPage(x))
}
