ui <- fillPage(
  fillRow(
    flex = c(NA, 1),
    div(
      textInput("title", label = "Title", value = "glop"),
      selectInput("series", "series", choices = c("series1", "series2", "series3"))
    ),
    mwModuleUI("ui", height = "100%")
  )
)

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
    range = mwSlider(2001, 2100, c(2001, 2050)),
    series = mwSharedValue(),
    title = mwSharedValue(), .runApp = FALSE,
    .compare = "range"
  )
  #
  mwModule("ui", c, title = reactive(input$title), series = reactive(input$series))
}

shinyApp(ui, server)
