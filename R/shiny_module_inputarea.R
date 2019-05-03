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
        checkboxInput(ns("compare"), "Compare", value = TRUE),
        shiny::conditionalPanel(
          sprintf("input['%s']", ns("compare")),
          shiny::selectInput(ns(".compareVars"), "Variables", choices = c(), multiple = TRUE),
          shiny::numericInput(ns("nbCharts"), "Number of charts",
                              value = 1, max = 12),
          shiny::selectInput(ns("ncols"), "Number of columns", c("auto", 1:4))
        )
      )
    )
  )
}

inputAreaModuleServer <- function(input, output, session, chartId, ctrl) {
  ns <- session$ns

  shiny::updateCheckboxInput(session, "compare", value = ctrl$ncharts > 1)
  shiny::updateNumericInput(session, "nbCharts", value = ctrl$ncharts)
  shiny::updateSelectInput(session, ".compareVars", choices = ctrl$uiSpec$getShareable(),
                           selected = intersect(ctrl$uiSpec$getShareable(), ctrl$uiSpec$inputList$unshared()))

  listeners <- c()
  visible <- reactive(input$visible())
  updateContent <- reactiveVal(0)

  # Controller initialization
  ctrl$init()
  ctrl$setShinySession(output, session)

  dim <- reactive({
    if (input$nbCharts == 1) {
      ncol <- 1
    } else if (input$ncols == "auto") {
      ncol <- NULL
    } else {
      ncol <- as.numeric(input$ncols)
    }
    append(.getRowAndCols(input$nbCharts, ncol = ncol),
           list(updateContent = updateContent()))
  })

  observeEvent(input$compare, {
    if (!input$compare) {
      shiny::updateNumericInput(session, "nbCharts", value = 1, min = 1)
      updateSelectInput(session, ".compareVars", selected = list())
    }
    else shiny::updateNumericInput(session, "nbCharts", value = max(ctrl$ncharts, 2), min = 2)
  }, ignoreInit = TRUE)

  addListener <- function(i) {
    id <- i$getID()
    e <- new.env()
    e$firstCall <- TRUE
    if (!is.character(id)) return()
    if (id %in% listeners) return()
    if (ctrl$inputList[id]$type != "sharedValue") {
      observeEvent(input[[id]], {
        if (e$firstCall) {
          e$firstCall <- FALSE
        } else {
          ctrl$setValueById(id, value = input[[id]])
          updateContent(updateContent() + 1)
        }
      })
      listeners <<- append(listeners, id)
    }
  }

  updateInputs <- function(chartId) {
    updateTextInput(session, "chartid", value = chartId)
    if (chartId == -1) {
      content <- ""
    } else {
      if (chartId == 0) {
        inputs <- ctrl$uiSpec$getInputsForChart(0)
        if (input$nbCharts == 1 && length(ctrl$uiSpec$inputList$unshared()) > 0) {
          inputs <- c(inputs, ctrl$uiSpec$getInputsForChart(1))
        }
      } else inputs <- ctrl$uiSpec$getInputsForChart(chartId)

      content <- shiny::tagList(lapply(inputs, function(x) {x$getHTML(ns)}))

      lapply(ctrl$uiSpec$inputList$inputTable$input, addListener)
    }

    output$inputarea <- shiny::renderUI(content)

    # Update visibility of inputs
    lapply(ctrl$inputList$inputTable$input, function(input) {
      # Update input visibility
      if (chartId != get(".id", envir = input$env)) return()

      # catIfDebug("Update visibility of", input$getID())
      # shiny::updateCheckboxInput(
      #   session,
      #   paste0(input$getID(), "_visible"),
      #   value = eval(input$display, envir = input$env)
      # )
      # Hack to fix https://github.com/rstudio/shiny/issues/1490
      if (input$type == "select" && identical(input$lastParams$multiple, TRUE)) {
        input$valueHasChanged <- TRUE
        input$updateHTML(session)
      }
    })
  }

  observeEvent(chartId(), {
    updateInputs(chartId())
  })

  observeEvent(input$.compareVars, ignoreNULL = FALSE, ignoreInit = TRUE,  {
    for (n in input$.compareVars) {
      ctrl$uiSpec$unshareInput(n)
    }

    for (n in setdiff(ctrl$uiSpec$getShareable(), input$.compareVars)) {
      newSharedInputs <- ctrl$uiSpec$shareInput(n)
      if (length(newSharedInputs) > 0 & input$compare) {
        for (i in 2:input$nbCharts) ctrl$updateChart(i)
      }
    }

    updateInputs(chartId())
    updateContent(updateContent() + 1)
  })

  return(dim)
}
