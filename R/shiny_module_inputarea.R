inputAreaModuleUI <- function(id) {
  ns <- NS(id)
  shiny::conditionalPanel(
    sprintf("input['%s']", ns("visible")),
    class = "mw-input-container",
    tags$div(style = "display:none;",
      shiny::checkboxInput(ns("visible"), label = "visible")
    ),
    tags$div(
      class ="mw-inputs",
      style = "display:block;",
      shiny::uiOutput(ns("inputarea")),
      shiny::conditionalPanel(
        sprintf("input['%s']", ns("visible")),
        checkboxInput(ns("compare"), "Compare"),
        shiny::conditionalPanel(
          sprintf("input['%s']", ns("compare")),
          shiny::numericInput(ns("nbCharts"), "Number of charts",
                              value = 2, min = 2, max = 12),
          shiny::selectInput(ns("ncols"), "Number of columns", c("auto", 1:4))
        )
      )
    )
  )
}

inputAreaModuleServer <- function(input, output, session, chartId) {
  ns <- session$ns

  visible <- reactive(input$visible())

  nbCharts <- reactive({print(input$compare);if (is.null(input$compare) || !input$compare) 1 else input$nbCharts})

  dim <- reactive({
    if (nbCharts() == 1) {
      ncol <- 1
    } else if (input$ncols == "auto") {
      ncol <- NULL
    } else {
      ncol <- as.numeric(input$ncols)
    }
    .getRowAndCols(nbCharts(), ncol = ncol)
  })

  observeEvent(chartId(), {
    if (chartId() == -1) {
      updateCheckboxInput(session, "visible", value = FALSE)
      content <- ""
    } else if (chartId() == 0) {
      updateCheckboxInput(session, "visible", value = TRUE)
      content <- ""
    } else {
      updateCheckboxInput(session, "visible", value = TRUE)
      content <- paste("inPuts for chart", chartId())
    }

    output$inputarea <- shiny::renderUI(content)
  })

  return(dim)
}
