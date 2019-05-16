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

  menuState <- callModule(manipulateWidget:::menuModuleServer, "menu", ncharts, nrow, ncol, displayIndBtns)

  chartId <- reactive(menuState()$chartId)

  observe({
    req(dim$n)
    ctrl$setChartNumber(dim$n, dim$nrow, dim$ncol)
    lapply(seq_len(dim$n), function(i) {
      # output[[paste0("output_", i)]] <- ctrl$renderFunc({
      #   ctrl$charts[[i]] %>%
      #     htmlwidgets::onRender(
      #       "function(el, x) {this.width = undefined; this.height = undefined}"
      #     )
      # })
    })
  })

  observeEvent(menuState()$done, {
    manipulateWidget:::onDone(ctrl)
  })
}
