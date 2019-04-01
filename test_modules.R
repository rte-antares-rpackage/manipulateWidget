library(shiny)
library(plotly)
library(dygraphs)

manipulateWidget::mwDebug()

mydata <- data.frame(year = 2000+1:100, value = rnorm(100))
ctrl <- manipulateWidget(dygraph(mydata[range[1]:range[2] - 2000, ], main = title),
                 range = mwSlider(2001, 2100, c(2001, 2100)),
                 title = mwText("Fictive time series"), .runApp = FALSE)
ctrl$init()

htmldep <- htmltools::htmlDependency(
  "manipulateWidget",
  "0.7.0",
  system.file("manipulate_widget", package = "manipulateWidget"),
  script = "manipulate_widget.js",
  style = "manipulate_widget.css"
)

ui <- fillPage(
  fillRow(
    flex = c(NA,NA, 1),
    fillCol(width = 50, manipulateWidget:::menuModuleUI("menu", updateBtn = FALSE)),
    manipulateWidget:::inputAreaModuleUI("inputarea"),
    fillCol(manipulateWidget:::gridModuleUI("grid"))
  )
)

ui <- htmltools::attachDependencies(ui, htmldep, TRUE)

server <- function(input, output, session) {
  dim <- callModule(manipulateWidget:::inputAreaModuleServer, "inputarea", chartId, ctrl)

  ncharts <- reactive(dim()$n)
  nrow <- reactive(dim()$nrow)
  ncol <- reactive(dim()$ncol)

  content <- reactive({
    lapply(seq_len(ncharts()), function(i) {
      ctrl$outputFunc(paste0("output_", i), width = "100%", height = "100%")
    })
  })

  callModule(manipulateWidget:::gridModuleServer, "grid", content = content, dim = dim)
  menuState <- callModule(manipulateWidget:::menuModuleServer, "menu", ncharts, nrow, ncol)

  chartId <- reactive(menuState()$chartId)

  observeEvent(dim(), {
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


runGadget(ui, server)
