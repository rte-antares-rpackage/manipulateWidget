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
          shiny::selectInput(ns(".compareVars"), "Variables", choices = c(), multiple = TRUE),
          shiny::numericInput(ns("nbCharts"), "Number of charts",
                              value = 2, min = 2, max = 12),
          shiny::selectInput(ns("ncols"), "Number of columns", c("auto", 1:4))
        )
      )
    )
  )
}

inputAreaModuleServer <- function(input, output, session, chartId, ctrl) {
  ns <- session$ns

  listeners <- c()
  visible <- reactive(input$visible())
  updateContent <- reactiveVal(0)
  nbCharts <- reactive({if (is.null(input$compare) || !input$compare) 1 else input$nbCharts})

  # Controller initialization
  ctrl$init()
  ctrl$setShinySession(output, session)

  observe({
    shiny::updateSelectInput(session, ".compareVars", choices = ctrl$uiSpec$getShareable())
  })

  dim <- reactive({
    if (nbCharts() == 1) {
      ncol <- 1
    } else if (input$ncols == "auto") {
      ncol <- NULL
    } else {
      ncol <- as.numeric(input$ncols)
    }
    append(.getRowAndCols(nbCharts(), ncol = ncol),
           list(updateContent = updateContent()))
  })

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
        if (nbCharts() == 1 && length(ctrl$uiSpec$inputList$unshared()) > 0) {
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

  # In case users cancels comparison mode, share all inputs
  observeEvent(nbCharts(), {
    if (nbCharts() == 1) {
      updateSelectInput(session, ".compareVars", selected = list())
    }
  })

  observeEvent(input$.compareVars, ignoreNULL = FALSE, ignoreInit = TRUE,  {
    for (n in input$.compareVars) {
      ctrl$uiSpec$unshareInput(n)
    }

    for (n in setdiff(ctrl$uiSpec$getShareable(), input$.compareVars)) {
      newSharedInputs <- ctrl$uiSpec$shareInput(n)
      if (length(newSharedInputs) > 0 & nbCharts() > 1) {
        for (i in 2:nbCharts()) ctrl$updateChart(i)
      }
    }

    updateInputs(chartId())
    updateContent(updateContent() + 1)
  })

  return(dim)
}
