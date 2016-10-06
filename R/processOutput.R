#Copyright © 2016 RTE Réseau de transport d’électricité

#' Prepare widgets and other types of objects to be displayed in the shiny
#' gadget
#' @noRd
.processOutput <- function(x) {

  if (is(x, "plotly_hash")) {
    if(requireNamespace("plotly")) {
      x <- plotly::as.widget(x)
      x$width <- x$height <- "100%"
      return(x)
    }
    else return("Package plotly is missing")
  }

  if (is(x, "datatables")) {
    # How to set size ?
    return(tags$div(htmltools::as.tags(x),
                    style = "width:100%;max-height:100%;overflow:auto"))
  }

  if (is(x, "htmlwidget")) {
    x$width <- x$height <- "100%"
    return(x)
  }

  x
}
