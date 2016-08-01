manipulateWidget <- function(expr, ..., main = NULL) {
  expr <- substitute(expr)

  if (is.null(main)) {
    main <- deparse(expr)
    if (nchar(main) > 53) {
      main <- substring(main, 1, 50)
      main <- paste0(main, "...")
    }
  }

  controls <- list(...)
  controlNames <- names(controls)

  controls <- mapply(function(f, id) f(id), f = controls, id = controlNames,
                     SIMPLIFY = FALSE, USE.NAMES = FALSE)

  ui <- miniPage(
    gadgetTitleBar(main),
    miniContentPanel(
      fillRow(flex = c(1, 3),
        fillCol(
          unname(controls)
        ),

        htmlOutput("content", style = "height:100%; width:100%")
      )
    )
  )

  server <- function(input, output, session) {

    inputList <- reactive({
      res <- lapply(controlNames, function(s) {
        eval(parse(text = paste0("input$", s)))
      })
      names(res) <- controlNames

      res
    })

    output$content <- renderUI({
      res <- eval(expr, envir = inputList())
      res$width <- res$height <- "100%"
      fillCol(res)
    })

    observeEvent(input$done, {
      stopApp(eval(expr, envir = inputList()))
    })
  }

  runGadget(ui, server)

}
