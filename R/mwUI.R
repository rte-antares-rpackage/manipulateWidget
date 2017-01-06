#Copyright © 2016 RTE Réseau de transport d’électricité

#' Generate the UI of a manipulateWidget gadget
#'
#' This function can be used if you desire to create a gadget that has the same
#' UI as a manipulateWidget gadget but with a custom server logic.
#'
#' @param .outputFun
#' The output function for the desired htmlwidget.
#' @param .titleBar
#' Whether to include a title bar with controls in the widget
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
                 .outputFun = NULL, .outputId = "output",
                 .titleBar = TRUE, .compare = NULL, .compareDir = c("v", "h"),
                 .controlList = NULL) {

  .controlPos <- match.arg(.controlPos)
  .compareDir <- match.arg(.compareDir)
  controls <- append(list(...), .controlList)

  if (is.null(.compare)) {
    commonControls <- controls

    if(is.null(.outputFun)) {
      .content <- htmlOutput(.outputId, style = "height:100%;width:100%")
    } else {
      .content <- .outputFun(.outputId, width = "100%", height = "100%")
    }
  } else {
    controls <- comparisonControls(controls, .compare)
    commonControls <- controls$common

    .content <- fillCol(
      mwUI(.controlList = controls$ind, .outputFun = .outputFun,
           .outputId = .outputId, .titleBar = FALSE),
      mwUI(.controlList = controls$ind2, .outputFun = .outputFun,
           .outputId = paste0(.outputId, "2"), .titleBar = FALSE)
    )
  }

  if (length(commonControls) == 0) {
    ui <- miniContentPanel(
      .content
    )
  } else if (.controlPos == "tab") {
    ctrls <- mwControlsUI(commonControls, .dir = "v", .n = .tabColumns,
                          .updateBtn = .updateBtn)
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
    ctrls <- mwControlsUI(commonControls, .dir = "v", .updateBtn = .updateBtn)
    ui <- miniContentPanel(
      fillRow(flex = c(NA, 1),
              tags$div(style ="width:200px;height:100%;overflow-y:auto;", ctrls),
              .content
      )
    )

  } else if (.controlPos == "top") {
    ctrls <- mwControlsUI(commonControls, .dir = "h",.updateBtn = .updateBtn)
    ui <- miniContentPanel(
      fillCol(flex = c(NA, 1),
              ctrls,
              .content
      )
    )

  } else if (.controlPos == "right") {
    ctrls <- mwControlsUI(commonControls, .dir = "v", .updateBtn = .updateBtn)
    ui <- miniContentPanel(
      fillRow(flex = c(1, NA),
              .content,
              tags$div(style ="width:200px;height:100%;overflow-y:auto;", ctrls)
      )
    )

  } else if (.controlPos == "bottom") {
    ctrls <- mwControlsUI(commonControls, .dir = "h", .updateBtn = .updateBtn)
    ui <- miniContentPanel(
      fillCol(flex = c(1, NA),
              .content,
              ctrls
      )
    )

  }

  if (.titleBar) {
    res <- miniPage(
      gadgetTitleBar(.main),
      ui
    )
  } else {
    res <- miniPage(
      ui
    )
  }

  res
}
