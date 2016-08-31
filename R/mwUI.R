#' Generate the UI of a manipulateWidget gadget
#'
#' This function can be used if you desire to create a gadget that has the same
#' UI as a manipulateWidget gadget but with a custom server logic.
#'
#' @inheritParams manipulateWidget
#'
#' @return
#' A \code{shiny.tag.list} object that can be used in function
#' \code{\link[shiny]{runGadget}} as ui parameter
#'
#' @export
#'
mwUI <- function(..., .controlPos = c("left", "top", "right", "bottom", "tab"),
                 .tabColumns = 2, .updateBtn = FALSE, .main = "",
                 .content = htmlOutput("content", style = "height:100%;width:100%")) {

  .controlPos <- match.arg(.controlPos)

  if (.controlPos == "tab") {

    ui <- miniTabstripPanel(
      miniTabPanel("Parameters", icon = icon("sliders"),
        miniContentPanel(
          mwControlsUI(..., .dir = "v", .n = .tabColumns, .updateBtn = .updateBtn)
        )
      ),
      miniTabPanel("Plot", icon = icon("area-chart"),
        miniContentPanel(
         .content
        )
      )
    )

  } else if (.controlPos == "left") {

    ui <- miniContentPanel(
      fillRow(flex = c(NA, 1),
              mwControlsUI(..., .dir = "v", .updateBtn = .updateBtn),
              .content
      )
    )

  } else if (.controlPos == "top") {

    ui <- miniContentPanel(
      fillCol(flex = c(NA, 1),
              mwControlsUI(..., .dir = "h",.updateBtn = .updateBtn),
              .content
      )
    )

  } else if (.controlPos == "right") {

    ui <- miniContentPanel(
      fillRow(flex = c(1, NA),
              .content,
              mwControlsUI(..., .dir = "v", .updateBtn = .updateBtn)
      )
    )

  } else if (.controlPos == "bottom") {

    ui <- miniContentPanel(
      fillCol(flex = c(1, NA),
              content,
              mwControlsUI(..., .dir = "h", .updateBtn = .updateBtn)
      )
    )

  }

  miniPage(
    gadgetTitleBar(.main),
    ui
  )
}
