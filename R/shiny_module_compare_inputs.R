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
      for (n in intersect(ctrl$uiSpec$getShareable(), input$.compareVars)) {
        ctrl$uiSpec$shareInput(n)
      }
      updateSelectInput(session, ".compareVars", selected = list())
    }
  }, ignoreInit = TRUE, ignoreNULL = FALSE)

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

  observeEvent(input$.compareVars, {
    toUnshare <- setdiff(input$.compareVars, ctrl$uiSpec$inputList$unshared())
    toShare <- setdiff(
      setdiff(ctrl$uiSpec$getShareable(), input$.compareVars),
      ctrl$uiSpec$inputList$shared()
    )

    for (n in toUnshare) {
      ctrl$uiSpec$unshareInput(n)
    }

    for (n in toShare) {
      newSharedInputs <- ctrl$uiSpec$shareInput(n)
      if (length(newSharedInputs) > 0 & nbCharts() > 1) {
        for (i in 2:nbCharts()) ctrl$updateChart(i)
      }
    }

    unshared <- intersect(ctrl$uiSpec$getShareable(), ctrl$uiSpec$inputList$unshared())
    if (!identical(sort(input$.compareVars), sort(unshared))) {
      shiny::updateSelectInput(session, ".compareVars", selected = unshared)
    }
  })

  res$.compareVars <- reactive(input$.compareVars)

  res
}
