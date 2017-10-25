ui <- fillPage(
  fillRow(
    flex = c(NA, 1),
    div(
      textInput("title", label = "Title", value = "glop"),
      sliderInput("obs", "Number of observations:",
                  min = 10, max = 1000, value = 500)
    ),
    mwModuleUI("ui", height = "100%")
  )
)

server <- function(input, output, session) {

  data <- reactive({
    if(runif(1) > 0.5){
      data.frame(
        year = 2000+1:input$obs,
        series1 = rnorm(input$obs),
        series2 = rnorm(input$obs),
        series3 = rnorm(input$obs)
      )
    } else {
      data.frame(
        year = 2000+1:input$obs,
        series1 = rnorm(input$obs),
        series2 = rnorm(input$obs)
      )
    }
  })

  c <- manipulateWidget(
    {
      dygraph(data[range[1]:range[2] - 2000, c("year", series)], main = title)
    },
    range = mwSlider(min = 2010,
                     max = 2001 + (nrow(data)-1), c(2001, 2001 + (nrow(data)-1))),
    series = mwSelect(choices = colnames(data)[-1],
                      value = {colnames(data)[3]}, .display = TRUE),
    title = mwSharedValue(),
    data = mwSharedValue(), .runApp = FALSE,
    .compare = "range"
  )
  mwModule("ui", c, title = reactive(input$title), data = data)
}

shinyApp(ui, server)
