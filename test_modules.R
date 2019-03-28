library(shiny)
library(plotly)

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
    fillCol(width = 50, menuModuleUI("menu", updateBtn = FALSE)),
    inputAreaModuleUI("inputarea"),
    fillCol(gridModuleUI("grid"))
  )
)

ui <- htmltools::attachDependencies(ui, htmldep, TRUE)

server <- function(input, output, session) {
  dim <- callModule(manipulateWidget:::inputAreaModuleServer, "inputarea", chartId)

  ncharts <- reactive(dim()$n)
  nrow <- reactive(dim()$nrow)
  ncol <- reactive(dim()$ncol)

  content <- reactive({
    lapply(seq_len(ncharts()), function(i) {
      plotlyOutput(paste0("output_", i), width = "100%", height = "100%")
    })
  })

  callModule(manipulateWidget:::gridModuleServer, "grid", content = content, dim = dim)
  chartId <- callModule(manipulateWidget:::menuModuleServer, "menu", ncharts, nrow, ncol)

  observe({
    lapply(seq_len(dim()$n), function(i) {
      output[[paste0("output_", i)]] <- renderPlotly({
        mydata <- data.frame(year = 2000+1:100, value = rnorm(100))
        plot_ly(mydata)%>%
          add_lines(~year, ~value) %>%
          htmlwidgets::onRender(
            "function(el, x) {this.width = undefined; this.height = undefined}"
          )
      })
    })
  })
}


runGadget(ui, server)
