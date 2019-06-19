mwModuleServer <- function(input, output, session, ctrl, ...) {
  ns <- session$ns

  ctrl <- ctrl$clone()

  dim <- callModule(manipulateWidget:::inputAreaModuleServer, "inputarea", chartId, ctrl)

  ncharts <- reactive(dim$n)
  nrow <- reactive(dim$nrow)
  ncol <- reactive(dim$ncol)
  displayIndBtns <- reactive(dim$displayIndBtns)


  shinyGridEnv <- callModule(manipulateWidget:::gridModuleServer, "grid", dim = dim, ctrl = ctrl)

  ctrl$setShinySession(shinyGridEnv$output, shinyGridEnv$session)

  menuState <- callModule(manipulateWidget:::menuModuleServer, "menu", ncharts, nrow, ncol, displayIndBtns, ctrl)

  chartId <- reactive(menuState()$chartId)

  observe({
    req(dim$n)
    ctrl$setChartNumber(dim$n, dim$nrow, dim$ncol)
  })

  observeEvent(menuState()$done, {
    onDone(ctrl)
  })

  observeEvent(menuState()$save, {
    print("save")
  })

  observeEvent(menuState()$update, {
    print("update")
  })
}
