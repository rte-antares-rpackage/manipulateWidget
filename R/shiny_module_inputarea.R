inputAreaModuleUI <- function(id) {
  ns <- NS(id)
  shiny::conditionalPanel(
    sprintf("input['%s'] != -1", ns("chartid")),
    class = "mw-input-container",
    tags$div(style = "display:none;",
      shiny::textInput(ns("chartid"), label = "chartid")
    ),
    tags$div(
      class ="mw-inputs",
      style = "display:block;",
      shiny::uiOutput(ns("inputarea")),
      shiny::conditionalPanel(
        sprintf("input['%s'] == '0'", ns("chartid")),
        checkboxInput(ns("compare"), "Compare"),
        shiny::conditionalPanel(
          sprintf("input['%s']", ns("compare")),
          shiny::selectInput(ns("variables"), "Variables", choices = c(), multiple = TRUE),
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

  nbCharts <- reactive({if (is.null(input$compare) || !input$compare) 1 else input$nbCharts})

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
    shiny::updateTextInput(session, "chartid", value = chartId())
    if (chartId() == -1) {
      content <- ""
    } else if (chartId() == 0) {
      content <- ""
    } else {
      content <- paste("Params for chart", chartId())
    }

    output$inputarea <- shiny::renderUI(content)
  })

  return(dim)
}
