mwModuleServer <- function(input, output, session, ctrl, ...) {
  ns <- session$ns

  ctrl <- ctrl$clone()

  reactiveValueList <- list(...)

  # If no reactive value, start immediately module.
  # Else delay start until outer inputs are initialized.
  if (length(reactiveValueList) == 0) startModule(ctrl)
  else {
    moduleStarted <- FALSE

    observe({
      for (n in names(reactiveValueList)) {
        ctrl$setValue(n, reactiveValueList[[n]](), reactive = TRUE)
      }
      if (!moduleStarted) {
        startModule(ctrl)
        moduleStarted <<- TRUE
      }
    })
  }

  return(ctrl)
}

startModule <- function(ctrl) {
  ctrl$init()

  dim <- callModule(inputAreaModuleServer, "inputarea", chartId, ctrl)

  ncharts <- reactive(dim$n)
  nrow <- reactive(dim$nrow)
  ncol <- reactive(dim$ncol)
  displayIndBtns <- reactive(dim$displayIndBtns)

  shinyGridEnv <- callModule(gridModuleServer, "grid", dim = dim, ctrl = ctrl)

  ctrl$setShinySession(shinyGridEnv$output, shinyGridEnv$session)

  menuState <- callModule(menuModuleServer, "menu", ncharts, nrow, ncol, displayIndBtns, ctrl)

  chartId <- reactive(menuState()$chartId)

  observe({
    req(dim$n)
    ctrl$setChartNumber(dim$n, dim$nrow, dim$ncol)
  })

  observeEvent(
    menuState()$done,
    onDone(ctrl)
  )

  observeEvent(
    menuState()$update,
    {
      if(!is.null(menuState()$update) && menuState()$update > 0){
        print("update")
        ctrl$updateCharts()
      }
    }
  )
}
