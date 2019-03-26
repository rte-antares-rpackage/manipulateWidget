htmldep <- htmltools::htmlDependency(
  "manipulateWidget",
  "0.7.0",
  system.file("manipulate_widget", package = "manipulateWidget"),
  script = "manipulate_widget.js",
  style = "manipulate_widget.css"
)

ui <- fillPage(
  fillRow(
    flex = c(NA, NA, 1),
    fillCol(width = 50, menuModuleUI("menu", updateBtn = FALSE)),
    fillCol(
      width = 100,
      sliderInput("ncells", "ncells", 1, 12, 1),
      textOutput("chartid")
    ),
    fillCol(gridModuleUI("grid"))
  )
)

ui <- htmltools::attachDependencies(ui, htmldep, TRUE)

server <- function(input, output, session) {
  print(class(input))

  content <- reactive({
    lapply(seq_len(input$ncells), function(i) {
      tags$div(
        style="width:100%;height:100%;",
        plotlyOutput(paste0("output_", i), width = "100%", height = "100%")
      )
    })
  })


  dim <- callModule(manipulateWidget:::gridModuleServer, "grid", content = content, NULL, NULL)

  ncharts <- reactive(dim()$n)
  nrow <- reactive(dim()$nrow)
  ncol <- reactive(dim()$ncol)


  chartId <- callModule(manipulateWidget:::menuModuleServer, "menu", ncharts, nrow, ncol)

  observe({
    output$chartid <- renderText(chartId())
  })

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
