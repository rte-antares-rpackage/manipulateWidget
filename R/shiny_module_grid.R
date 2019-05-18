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

  ncells <- reactiveVal(NULL)

  observeEvent(dim$n, {
    if (is.null(ncells())) {
      outputEls <- lapply(seq_len(dim$n), function(i) {
        content <- ctrl$outputFunc(ns(paste0("output_", i)), width = "100%", height = "100%")
        style <- sprintf("float:left;width:%s%%;height:%s%%;",
                         floor(100 / dim$ncol), floor(100 / dim$nrow))
        tags$div(class="mw-chart", style = style, content)
      })
      output$cells <- renderUI(shiny::tagList(outputEls))
    } else if (ncells() < dim$n) {
      outputEls <- lapply((ncells()+1):dim$n, function(i) {
        content <- ctrl$outputFunc(ns(paste0("output_", i)), width = "100%", height = "100%")
        style <- sprintf("float:left;width:%s%%;height:%s%%;",
                         floor(100 / dim$ncol), floor(100 / dim$nrow))
        tags$div(class="mw-chart", style = style, content)
      })
      shiny::insertUI(paste0("#",ns("cells")),ui=shiny::tagList(outputEls), session = session)
      resetSize(dim$nrow, dim$ncol, ns)
    } else if (ncells() > dim$n) {
      for (i in ncells():(dim$n+1)) {
        shiny::removeUI(sprintf("div:has(> #%s_%s)", ns("output"), i),session = session)
      }
    }
    ncells(dim$n)
  }, ignoreNULL = TRUE)

  observe({
    resetSize(dim$nrow, dim$ncol, ns)
    shinyjs::runjs("resizeAllWidgets()")
  })

  return(list(output = output, session = session))
}

resetSize <- function(nrow, ncol, ns) {
  width <- paste0(floor(100 / ncol), "%")
  height <- paste0(floor(100 / nrow), "%")
  id <- ns("cells")
  js <- sprintf(
    "$('#%s .mw-chart').css({'float':'left', 'width':'%s', 'height':'%s'})",
    id, width, height
  )
  shinyjs::runjs(js)
}
