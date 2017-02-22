#Copyright © 2016 RTE Réseau de transport d’électricité

#' Generate the UI of a manipulateWidget gadget
#'
#' This function can be used if you desire to create a gadget that has the same
#' UI as a manipulateWidget gadget but with a custom server logic.
#'
#' @param controls Object returned by preprocessControls
#' @param .outputFun The output function for the desired htmlwidget.
#' @param .outputId Id of the output element in the shiny interface.
#' @param .titleBar Whether to include a title bar with controls in the widget
#' @param .controlList List of input controls. This is an alternative to
#'   specifying directly the controls through the \code{...} arguments.
#' @param .container tag function that will be used to enclose the UI.
#' @param .style CSS style to apply to the container element.
#' @param .env Environment used to evaluate the inital values of controls. This
#'   parameter may have an impact on the result only when \code{.updateInputs}
#'   is used.
#' @inheritParams manipulateWidget
#'
#' @return
#' A \code{shiny.tag.list} object that can be used in function
#' \code{\link[shiny]{runGadget}} as ui parameter.
#'
#' @noRd
#'
mwUI <- function(.controlList, .controlPos = c("left", "top", "right", "bottom", "tab"),
                 .tabColumns = 2, .updateBtn = FALSE, .main = "",
                 .outputFun = NULL, .outputId = "output",
                 .titleBar = TRUE, .updateInputs = NULL, nmod = 1, .compareLayout = c("v", "h"),
                 .container = miniUI::miniContentPanel,
                 .style = "") {

  .controlPos <- match.arg(.controlPos)
  .compareLayout <- match.arg(.compareLayout)

  commonControls <- .controlList$shared

  if (nmod == 1) {
    if(is.null(.outputFun)) {
      .content <- shiny::htmlOutput(.outputId, style = "height:100%;width:100%")
    } else {
      .content <- .outputFun(.outputId, width = "100%", height = "100%")
    }
  } else {

    if (.compareLayout == "v") {
      .content <- shiny:: fillCol(
        mwUI(.controlList = list(shared = .controlList$ind[[1]]),
             nmod = 1, .outputFun = .outputFun,
             .outputId = .outputId, .titleBar = FALSE, .container=shiny:: fillRow,
             .style = "margin-left:5px; padding: 0 0 5px 5px;border-left: solid 1px #ddd;"),
        mwUI(.controlList = list(shared = .controlList$ind[[2]]),
             nmod = 1, .outputFun = .outputFun,
             .outputId = paste0(.outputId, "2"), .titleBar = FALSE,
             .container=shiny:: fillRow,
             .style = "margin-left:5px; padding: 5px 0 0 5px;border-left: solid 1px #ddd;")
      )
    } else {
      .content <- shiny:: fillRow(
        mwUI(.controlList = list(shared = .controlList$ind[[1]]),
             nmod = 1, .outputFun = .outputFun,
             .outputId = .outputId, .titleBar = FALSE, .controlPos = "top",
             .container=shiny:: fillRow,
             .style = "margin-left:5px;padding-left:5px;border-left: solid 1px #ddd;"),
        mwUI(.controlList = list(shared = .controlList$ind[[2]]),
             nmod = 1, .outputFun = .outputFun,
             .outputId = paste0(.outputId, "2"), .titleBar = FALSE, .controlPos = "top",
             .container = shiny:: fillRow, .style = "padding-left:5px;")
      )
    }
  }

  if (length(commonControls) == 0) {
    ui <- .container(
      .content,
      style = .style
    )
  } else if (.controlPos == "tab") {
    ctrls <- mwControlsUI(commonControls, .dir = "v", .n = .tabColumns,
                          .updateBtn = .updateBtn)
    ui <- miniTabstripPanel(
      miniTabPanel("Parameters", icon = shiny::icon("sliders"),
        miniContentPanel(
          ctrls
        )
      ),
      miniTabPanel("Plot", icon = shiny::icon("area-chart"),
        miniContentPanel(
         .content
        )
      )
    )

  } else if (.controlPos == "left") {
    ctrls <- mwControlsUI(commonControls, .dir = "v", .updateBtn = .updateBtn)
    ui <- .container(
      style = .style,
      shiny:: fillRow(flex = c(NA, 1),
              tags$div(style ="width:200px;height:100%;overflow-y:auto;", ctrls),
              .content
      )
    )

  } else if (.controlPos == "top") {
    ctrls <- mwControlsUI(commonControls, .dir = "h",.updateBtn = .updateBtn)
    ui <- .container(
      style = .style,
      shiny:: fillCol(flex = c(NA, 1),
              ctrls,
              .content
      )
    )

  } else if (.controlPos == "right") {
    ctrls <- mwControlsUI(commonControls, .dir = "v", .updateBtn = .updateBtn)
    ui <- .container(
      style = .style,
      shiny:: fillRow(flex = c(1, NA),
              .content,
              tags$div(style ="width:200px;height:100%;overflow-y:auto;", ctrls)
      )
    )

  } else if (.controlPos == "bottom") {
    ctrls <- mwControlsUI(commonControls, .dir = "h", .updateBtn = .updateBtn)
    ui <- .container(
      style = .style,
      shiny:: fillCol(flex = c(1, NA),
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
