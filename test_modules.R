library(shiny)
library(plotly)
library(dygraphs)

manipulateWidget::mwUndebug()

mydata <- data.frame(year = 2000+1:100, value = rnorm(100))
ctrl <- manipulateWidget(dygraph(mydata[range[1]:range[2] - 2000, ], main = title),
                 range = mwSlider(2001, 2100, c(2001, 2100)),
                 title = mwText("Fictive time series"), .runApp = FALSE)

ui <- fillPage(
  manipulateWidget:::mwUI("mw", updateBtn = FALSE, fillPage = FALSE, height = "100%")
)

server <- function(input, output, session) {
  callModule(manipulateWidget:::mwModuleServer, "mw", ctrl)
}


res <- runGadget(ui, server)
