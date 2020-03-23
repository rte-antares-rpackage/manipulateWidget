inputAreaModuleUI <- function(id, allowCompare = TRUE) {
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
      tags$div(
        shiny::textOutput(ns("input_title")),
        class="input-title"
      ),
      tags$div(
        class = "mw-inputarea",
        shiny::uiOutput(ns("inputarea"))
      ),
      shiny::conditionalPanel(
        sprintf("input['%s'] == '0'", ns("chartid")),
        class = "mw-inputarea",
        compareInputsModuleUI(ns("compare"), allowCompare = allowCompare)
      )
    )
  )
}

inputAreaModuleServer <- function(input, output, session, chartId, ctrl) {
  ns <- session$ns

  compareMod <- shiny::callModule(compareInputsModuleServer, "compare", ctrl)

  listeners <- c()
  visible <- reactive(input$visible())

  # Controller initialization
  ctrl$setShinyInputSession(session)

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
        }
      }, ignoreNULL = FALSE)
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
        if (compareMod$n == 1 && length(ctrl$uiSpec$inputList$unshared()) > 0) {
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
      # Hack to fix https://github.com/rstudio/shiny/issues/1490
      if (input$type == "select" && identical(input$lastParams$multiple, TRUE)) {
        input$valueHasChanged <- TRUE
        input$updateHTML(session)
      }
    })
  }

  observeEvent(chartId(), {
    updateInputs(chartId())

    if (chartId() == -1) title <- ""
    else if (chartId() == 0) title <- ctrl$translations$settings
    else title <- paste(ctrl$translations$chart, chartId())
    output$input_title <- shiny::renderText(title)
  })

  observeEvent(compareMod$.compareVars(), ignoreNULL = FALSE, ignoreInit = TRUE,  {
    updateInputs(chartId())
  })

  res <- reactiveValues()
  observe(res$n <- compareMod$n)
  observe(res$ncol <- compareMod$ncol)
  observe(res$nrow <- compareMod$nrow)
  observe(res$displayIndBtns <- length(compareMod$.compareVars()) > 0)

  res
}
