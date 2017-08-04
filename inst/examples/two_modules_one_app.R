mydata <- data.frame(
  year = 2000+1:100,
  series1 = rnorm(100),
  series2 = rnorm(100),
  series3 = rnorm(100)
)

c <- manipulateWidget(
  combineWidgets(dygraph(mydata[range[1]:range[2] - 2000, c("year", series)], main = title)),
  range = mwSlider(2001, 2100, c(2001, 2100)),
  series = mwSelect(c("series1", "series2", "series3")),
  title = mwText("Fictive time series"),
  .compare = c("title", "series"), .runApp = FALSE
)$init()

dt <- data.frame (
  x = sort(runif(100)),
  y = rnorm(100)
)

myPlot <- function(type, lwd) {
  if (type == "points") {
    plot_ly(dt, x= ~x, y = ~y, type = "scatter", mode = "markers")
  } else {
    plot_ly(dt, x= ~x, y = ~y, type = "scatter", mode = "lines", line = list(width = lwd))
  }
}

c2 <- manipulateWidget(
  combineWidgets(myPlot(type, lwd)),
  type = mwSelect(c("points", "lines"), "points"),
  lwd = mwSlider(1, 10, 1, .display = type == "lines"), .runApp = FALSE
)$init()

mwModuleInput <- c$getModuleUI(gadget = FALSE, saveBtn = TRUE)
mwModule <- c$getModuleServer()

mwModuleInput2 <- c2$getModuleUI(gadget = FALSE, saveBtn = TRUE)
mwModule2 <- c2$getModuleServer()

ui <- fillPage(
  fillRow(
    tags$div(mwModuleInput("pane1"), style = 'height:100%;'),
    tags$div(mwModuleInput2("pane2"), style = 'height:100%;')
  )
)

ui <- navbarPage("antaresViz",
                 tabPanel("prodStack",
                          tags$div(mwModuleInput("pane1"), style = 'height:800px;')
                 ),
                 tabPanel("exchangesStack",
                          tags$div(mwModuleInput2("pane2"), style = 'height:800px;')
                 ),
                 tabPanel("Table")
)
server <- function(input, output, session) {
  callModule(mwModule, "pane1")
  callModule(mwModule2, "pane2")
}

shinyApp(ui, server)
