compareInputsModuleUI <- function(id) {
  ns <- NS(id)
  shiny::uiOutput(ns("content"))
}

compareInputsModuleServer <- function(input, output, session, ctrl) {
  ns <- session$ns

  output$content <- shiny::renderUI({
    shiny::tagList(
      checkboxInput(ns("compare"), "Compare", value = ctrl$ncharts > 1),
      shiny::conditionalPanel(
        sprintf("input['%s']", ns("compare")),
        shiny::selectInput(
          ns(".compareVars"), "Variables",
          choices = ctrl$uiSpec$getShareable(),
          selected = intersect(ctrl$uiSpec$getShareable(), ctrl$uiSpec$inputList$unshared()),
          multiple = TRUE
        ),
        shiny::numericInput(ns("nbCharts"), "Number of charts",
                            value = max(2, ctrl$ncharts), min = 2, max = 12),
        shiny::selectInput(ns("ncols"), "Number of columns", c("auto", 1:4))
      )
    )
  })


  nbCharts <- reactive(if (input$compare) input$nbCharts else 1)

  observeEvent(input$compare, {
    if (!is.null(input$compare) & !input$compare) {
     #updateSelectInput(session, ".compareVars", selected = list())
    }
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  res <- reactiveValues()

  res$dim <- reactive({
    req(input$nbCharts)
    if (nbCharts() == 1) {
      ncol <- 1
    } else if (input$ncols == "auto") {
      ncol <- NULL
    } else {
      ncol <- as.numeric(input$ncols)
    }
    .getRowAndCols(nbCharts(), ncol = ncol)
  })

  res$.compareVars <- reactive(input$.compareVars)

  res
}
