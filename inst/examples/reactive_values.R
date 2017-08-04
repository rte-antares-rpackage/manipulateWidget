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
  range = mwSlider(2001, 2100, c(2001, 2100)),
  series = mwSharedValue("series1"),
  title = mwSharedValue("Fictive time series"), .runApp = FALSE
)$init()

mwModuleInput <- c$getModuleUI(gadget = FALSE, saveBtn = TRUE)
mwModule <- c$getModuleServer()

ui <- fillPage(
  fillRow(
    flex = c(NA, 1),
    div(
      textInput("title", label = "Title", value = "glop"),
      selectInput("series", "series", choices = c("series1", "series2", "series3"))
    ),
    mwModuleInput("ui")
  )
)

server <- function(input, output, session) {
  callModule(mwModule, "ui", series = reactive(input$series), title = reactive(input$title))
}

shinyApp(ui, server)
