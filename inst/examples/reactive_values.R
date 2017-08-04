mydata <- data.frame(
  year = 2000+1:100,
  series1 = rnorm(100),
  series2 = rnorm(100),
  series3 = rnorm(100)
)

c <- manipulateWidget(
  {
    print(title)
    if (is.null(series)) series <- "series1"
    if (is.null(title)) title <- ""
    dygraph(mydata[range[1]:range[2] - 2000, c("year", series)], main = title)
  },
  range = mwSlider(2001, 2100, c(2001, 2100)),
  series = mwSharedValue(),
  title = mwSharedValue(), .runApp = FALSE
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
    #uiOutput("ui", container = function(...) tags$div(style="height:100%;", ...))
  )
)

server <- function(input, output, session) {
  #
  # c$init()
  # mwModuleInput <- c$getModuleUI(gadget = FALSE, saveBtn = TRUE)
  # mwModule <- c$getModuleServer()
  # id <- paste0("mwModule_", sample(1e9, 1))
  # output$ui <- renderUI(mwModuleInput(id, height = "100%"))
  callModule(mwModule, "ui", series = reactive(input$series), title = reactive(input$title))
}

shinyApp(ui, server)






