library(dygraphs)
library(plotly)
library(shiny)


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
  title = mwText("Fictive time series"), .updateBtn = FALSE, .updateBtnInit = TRUE,
  .compare = c("title", "series"), .runApp = FALSE
)

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
  type = mwSelect(c("points", "lines"), "points"), .saveBtn = TRUE,
  .exportBtn = TRUE, .updateBtn = TRUE, .updateBtnInit = FALSE,
  lwd = mwSlider(1, 10, 1, .display = type == "lines"), .runApp = FALSE
)


c2 <- manipulateWidget(
  plotEnergyUse(Country, Period),
  Period = mwSlider(1960, 2014, c(1960, 2014)),
  Country = mwSelect(sort(unique(worldEnergyUse$country))),
  .compare = list(Country = c("United States", "China")),
  .compareOpts = compareOptions(ncol = 2), .runApp = FALSE
)


ui <- navbarPage(
  "Test manipulateWidget",
  tabPanel(
    "Module 1",
    mwModuleUI("mod1", height = "500px", saveBtn = T, updateBtn = F, exportBtn = T),
    mwModuleUI("mod3", height = "500px", saveBtn = T, updateBtn = T, exportBtn = T)
  ),
  tabPanel(
    "Module 2",
    mwModuleUI("mod2", height = "500px", saveBtn = T, updateBtn = T, exportBtn = T)
  )
)

server <- function(input, output, session) {
  mwModule("mod1", c)
  mwModule("mod2", c2)
}

shinyApp(ui, server)
