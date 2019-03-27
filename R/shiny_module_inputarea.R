inputAreaModuleUI <- function(id) {
  ns <- NS(id)
  shiny::uiOutput(ns("inputarea"))
}

inputAreaModuleServer <- function(input, output, session, chartId) {
  ns <- session$ns

  compare_block <- shiny::tagList(
    checkboxInput(ns("compare"), "Compare"),
    shiny::conditionalPanel(
      sprintf("input['%s']", ns("compare")),
      "compare panel"
    )
  )

  observeEvent(chartId(), {
    if (chartId() == -1) content <- tags$div()
    else if (chartId() == 0) {
      content <- tags$div(
        class = "mw-inputs-container",
        compare_block
      )
    } else {
      content <- tags$div(
        class = "mw-inputs",
        paste("inPuts for chart", chartId())
      )
    }

    output$inputarea <- shiny::renderUI(content)
  })
}
