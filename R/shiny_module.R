mwModuleServer <- function(input, output, session, ctrl) {
  ns <- session$ns

  ctrl <- ctrl$clone()
  ctrl$init()

  dim <- callModule(manipulateWidget:::inputAreaModuleServer, "inputarea", chartId, ctrl)

  ncharts <- reactive(dim()$n)
  nrow <- reactive(dim()$nrow)
  ncol <- reactive(dim()$ncol)

  content <- reactive({
    lapply(seq_len(ncharts()), function(i) {
      ctrl$outputFunc(ns(paste0("output_", i)), width = "100%", height = "100%")
    })
  })

  callModule(manipulateWidget:::gridModuleServer, "grid", content = content, dim = dim)
  menuState <- callModule(manipulateWidget:::menuModuleServer, "menu", ncharts, nrow, ncol)

  chartId <- reactive(menuState()$chartId)

  observeEvent(dim(), {
    print(dim())
    ctrl$setChartNumber(dim()$n, dim()$nrow, dim()$ncol)
    lapply(seq_len(dim()$n), function(i) {
      output[[paste0("output_", i)]] <- ctrl$renderFunc({
        ctrl$charts[[i]] %>%
          htmlwidgets::onRender(
            "function(el, x) {this.width = undefined; this.height = undefined}"
          )
      })
    })
  })

  observeEvent(menuState()$done, {
    manipulateWidget:::onDone(ctrl)
  })
}
