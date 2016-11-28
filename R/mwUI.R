#Copyright © 2016 RTE Réseau de transport d’électricité

#' Generate the UI of a manipulateWidget gadget
#'
#' This function can be used if you desire to create a gadget that has the same
#' UI as a manipulateWidget gadget but with a custom server logic.
#'
#' @param .content
#' HTML of the main content of the application, where outputs are displayed.
#' @inheritParams manipulateWidget
#'
#' @return
#' A \code{shiny.tag.list} object that can be used in function
#' \code{\link[shiny]{runGadget}} as ui parameter.
#'
#' @export
#'
mwUI <- function(..., .controlPos = c("left", "top", "right", "bottom", "tab"),
                 .tabColumns = 2, .updateBtn = FALSE, .main = "",
                 .content = htmlOutput("output", style = "height:100%;width:100%")) {

  .controlPos <- match.arg(.controlPos)

  if (.controlPos == "tab") {
    ctrls <- mwControlsUI(..., .dir = "v", .n = .tabColumns, .updateBtn = .updateBtn)
    ui <- miniTabstripPanel(
      miniTabPanel("Parameters", icon = icon("sliders"),
        miniContentPanel(
          ctrls
        )
      ),
      miniTabPanel("Plot", icon = icon("area-chart"),
        miniContentPanel(
         .content
        )
      )
    )

  } else if (.controlPos == "left") {
    ctrls <- mwControlsUI(..., .dir = "v", .updateBtn = .updateBtn)
    ui <- miniContentPanel(
      fillRow(flex = c(NA, 1),
              tags$div(style ="width:200px;height:100%;overflow-y:auto;", ctrls),
              .content
      )
    )

  } else if (.controlPos == "top") {
    ctrls <- mwControlsUI(..., .dir = "h",.updateBtn = .updateBtn)
    ui <- miniContentPanel(
      fillCol(flex = c(NA, 1),
              ctrls,
              .content
      )
    )

  } else if (.controlPos == "right") {
    ctrls <- mwControlsUI(..., .dir = "v", .updateBtn = .updateBtn)
    ui <- miniContentPanel(
      fillRow(flex = c(1, NA),
              .content,
              tags$div(style ="width:200px;height:100%;overflow-y:auto;", ctrls)
      )
    )

  } else if (.controlPos == "bottom") {
    ctrls <- mwControlsUI(..., .dir = "h", .updateBtn = .updateBtn)
    ui <- miniContentPanel(
      fillCol(flex = c(1, NA),
              .content,
              ctrls
      )
    )

  }

  res <- miniPage(
    gadgetTitleBar(.main),
    ui
  )
  attr(res, "controlNames") <- .getControlNames(ctrls)
  res
}
