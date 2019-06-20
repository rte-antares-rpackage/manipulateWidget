#' Private function that generates the general layout of the application
#'
#' @param ns namespace function created with shiny::NS(). Useful to create
#'   modules.
#' @param inputs Object returned by preprocessInputs
#' @param ncol Number of columns in the chart area.
#' @param nrow Number of rows in the chart area.
#' @param outputFun Function that generates the html elements that will contain
#'   a given widget
#' @param okBtn Should the OK Button be added to the UI ?
#' @param saveBtn Should an save button be added to the controls ? For saving output as html. Does not work in RStudio Viewer
#' @param exportBtn Should an export button be added to the controls ? For saving output as png. Does not work in RStudio Viewer
#' @param exportType \code{.exportBtn}, using \code{html2canvas} (default) and keeping current zoom, ... or using \code{webshot}
#' @param updateBtn Should the updateBtn be added to the UI ? Currently unused.
#' @param width, height	Must be a valid CSS unit (like "100%", "400px", "auto") or a number,
#' which will be coerced to a string and have "px" appended. Default to "100%" & "400px"
#'
#' @return shiny tags
#'
#' @noRd
mwUI <- function(id, nrow = 1, ncol = 1, okBtn = TRUE,
                 saveBtn = TRUE, exportBtn = TRUE, exportType = "html2canvas", updateBtn = FALSE,
                 areaBtns = TRUE, border = FALSE, width = "100%", height = "400px",
                 fillPage = TRUE, showCompare = TRUE) {

  ns <- NS(id)
  htmldep <- htmltools::htmlDependency(
    "manipulateWidget",
    "0.7.0",
    system.file("manipulate_widget", package = "manipulateWidget"),
    script = "manipulate_widget.js",
    style = "manipulate_widget.css"
  )

  if(exportBtn & (exportType %in% "html2canvas")) {

    fileSaver_dep <- htmltools::htmlDependency(
      name = "FileSaver",
      version = "1.1.20151003",
      src = c(file=system.file("lib/export/FileSaver", package="manipulateWidget")),
      script = "FileSaver.min.js"
    )

    Blob_dep <- htmltools::htmlDependency(
      name = "Blob",
      version = "1.0",
      src = c(file=system.file("lib/export/Blob", package="manipulateWidget")),
      script = "Blob.js"
    )

    canvastoBlob_dep <- htmltools::htmlDependency(
      name = "canvas-toBlob",
      version = "1.0",
      src = c(file=system.file("lib/export/canvas-toBlob", package="manipulateWidget")),
      script = "canvas-toBlob.js"
    )

    html2canvas_dep <- htmltools::htmlDependency(
      name = "html2canvas",
      version = "1.0",
      src = c(file=system.file("lib/export/html2canvas", package="manipulateWidget")),
      script = "html2canvas.js"
    )

    htmldep <- list(htmldep, fileSaver_dep, Blob_dep, canvastoBlob_dep, html2canvas_dep)
  }

  if (border) class <- "mw-container with-border"
  else class <- "mw-container"

  content <- fillRow(
    flex = c(NA, NA, 1),
    width = width, height = height,
    menuModuleUI(ns("menu"), updateBtn = updateBtn, saveBtn = saveBtn,
                 okBtn = okBtn, exportBtn = exportBtn, exportType = exportType),
    inputAreaModuleUI(ns("inputarea")),
    gridModuleUI(ns("grid"))
  )

  if(fillPage){
    container <- fillPage(
      shinyjs::useShinyjs(),
      tags$div(
        id = ns("ui"),
        class = class,
        style = paste("width:", width, ";height:", height, ";", sep = ""),
        content
      )
    )
  } else {
    container <- tags$div(
      id = ns("ui"),
      class = class,
      style = paste("width:", width, ";height:", height, ";", sep = ""),
      shinyjs::useShinyjs(),
      content
    )
  }

  htmltools::attachDependencies(container, htmldep, TRUE)
}


#' @param border Should a border be added to the module ?
#' @param okBtn Should the UI contain the OK button ?
#' @param saveBtn Should the UI contain the save button ? For saving output as html
#' @param exportBtn Should an export button be added to the controls ? For saving output as png
#' @param margin Margin to apply around the module UI. Should be one two or four valid css
#'   units.
#' @param width Width of the module UI.
#' @param height Height of the module UI.
#' @param header	Tag or list of tags to display as a common header above all tabPanels.
#' @param footer	Tag or list of tags to display as a common footer below all tabPanels
#'
#' @rdname mwModule
#' @export
mwModuleUI <- function(id, border = TRUE, okBtn = FALSE, saveBtn = TRUE, exportBtn = TRUE,
                       margin = 0, width = "100%", height = 400, header = NULL, footer = NULL) {
  res <- mwUI(id, border = border, okBtn = okBtn, saveBtn = saveBtn, exportBtn = exportBtn,
              width = width, height = height, fillPage = FALSE)
  shiny::tagList(
    header,
    res,
    footer
  )
}
