gridModuleUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("cells"), container = function(...) {
    tags$div(
      class = "mw-chartarea",
      ...
    )
  })
}

gridModuleServer <- function(input, output, session, dim, ctrl, ...) {
  ns <- session$ns

  content <- reactive({
    lapply(seq_len(dim()$n), function(i) {
      ctrl$outputFunc(ns(paste0("output_", i)), width = "100%", height = "100%")
    })
  })

  observe({
    outputEls <- lapply(seq_len(dim()$nrow * dim()$ncol), function(i) {
      if (i > length(content())) return(tags$div())
      style <- sprintf("float:left;width:%s%%;height:%s%%;",
                       floor(100 / dim()$ncol), floor(100 / dim()$nrow))
      tags$div(class="mw-chart", content()[[i]], style = style)
    })

    output$cells <- renderUI(shiny::tagList(outputEls))
  })

  return(list(output = output, session = session))
}
