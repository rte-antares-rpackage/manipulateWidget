#' UI of the gadget
#'
#' Creates the UI of the generated shiny gadget.
#'
#' @param controls
#'   list of shiny controls
#' @param position
#'   Where to place the controls ?
#' @param tabColumns
#'   Number of columns to create if position = tab
#'
#' @return
#' shiny gadget UI.
#'
#' @noRd
.ui <- function(controls, position, tabColumns) {

  if (position == "tab") {

    nrows <- ceiling(length(controls) / tabColumns)
    controlRows <- lapply(1:nrows, function(i) {
      do.call(fillRow, controls[((i-1) * tabColumns + 1):(i * tabColumns)])
    })

    controlUI <- do.call(fillCol, controlRows)

    ui <- miniTabstripPanel(
      miniTabPanel("Parameters", icon = icon("sliders"),
        miniContentPanel(
          controlUI
        )
      ),
      miniTabPanel("Plot", icon = icon("area-chart"),
        miniContentPanel(
          htmlOutput("content", style = "height:100%; width:100%")
        )
      )
    )

  } else if (position == "left") {

    ui <- miniContentPanel(
      fillRow(flex = c(1, 3),
        controls,
        htmlOutput("content", style = "height:100%; width:100%")
      )
    )

  } else if (position == "top") {

    controls$height <- "100px"

    ui <- miniContentPanel(
      fillCol(flex = c(NA, 1),
        do.call(fillRow, controls),
        htmlOutput("content", style = "height:100%; width:100%")
      )
    )

  } else if (position == "right") {

    ui <- miniContentPanel(
      fillRow(flex = c(3, 1),
        htmlOutput("content", style = "height:100%; width:100%"),
        controls
      )
    )

  } else if (position == "bottom") {

    controls$height <- "100px"

    ui <- miniContentPanel(
      fillCol(flex = c(1, NA),
        htmlOutput("content", style = "height:100%; width:100%"),
        do.call(fillRow, controls)
      )
    )

  }

  ui
}
