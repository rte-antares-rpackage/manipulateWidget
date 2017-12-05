require(manipulateWidget)
require(dygraphs)

ui <- fillPage(
  fillRow(
    flex = c(NA, 1),
    div(
      textInput("title", label = "Title", value = "glop"),
      selectInput("series", "series", choices = c("series1", "series2", "series3"))
    ),
    mwModuleUI("ui", height = "400px")
  )
)

range = 2001
server <- function(input, output, session) {
  mydata <- data.frame(
    year = 2000+1:100,
    series1 = rnorm(100),
    series2 = rnorm(100),
    series3 = rnorm(100)
  )

  c <- manipulateWidget(
    {
      dygraph(mydata[range[1]:range[2] - 2000, c("year", series)], main = title)
    },
    range = mwSlider(range, 2100, c(2010, 2050)),
    series = mwSharedValue(),
    title = mwSharedValue(
      {"init"}
    ), .runApp = FALSE,
    .compare = "range"
  )

  titre <- reactive({
    input$title
  })

  mwModule("ui", c, title = titre, series = reactive(input$series))
}

shinyApp(ui, server)
