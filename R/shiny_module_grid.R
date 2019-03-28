gridModuleUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("cells"), container = function(...) {div(style = "height:100%;width:100%;position:absolute;", ...)})
}

gridModuleServer <- function(input, output, session, content, dim, ...) {

  observe({
    outputEls <- lapply(seq_len(dim()$nrow * dim()$ncol), function(i) {
      if (i > length(content())) return(tags$div())
      style <- sprintf("float:left;width:%s%%;height:%s%%;border:solid 1px black;",
                       floor(100 / dim()$ncol), floor(100 / dim()$nrow))
      tags$div(class="mw-chart", content()[[i]], style = style)
    })

    output$cells <- renderUI(shiny::tagList(outputEls))
  })

  return(dim)
}
