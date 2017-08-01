#' Private function that returns a shiny server function to use in manipulateWidget
#'
#' @param .expr see manipulateWidget
#' @param controls Object returned by function preprocessControls
#' @param widgets A list of the widgets to show, in their initial state
#' @param renderFunction Function to use to render the widgets
#' @param .display see manipulateWidget
#' @param .compareLayout see manipulateWidget
#' @param .updateBtn see manipulateWidget
#'
#' @return A server function that can be used in runGadget.
#'
#' @noRd
#'
mwServer <- function(.expr, controls, widgets,
                     renderFunction,
                     .updateBtn, .return, nrow, ncol, useCombineWidgets) {


  function(input, output, session) {
    message("Click on the 'OK' button to return to the R session.")
    # Ensure that initial values of select inputs with multiple = TRUE are in
    # same order than the user asked.
    selectInputList <- subset(controls$desc, type == "select" & multiple)
    for (i in seq_len(nrow(selectInputList))) {
      shiny::updateSelectInput(
        session,
        selectInputList$name[i],
        selected = selectInputList$initValues[[i]]
      )
    }

    updateModule <- function(i) {
      # Initialize the widgets with their first evaluation
      if (useCombineWidgets) widgets[[i]] <- combineWidgets(widgets[[i]])
      output[[paste0("output", i)]] <- renderFunction(widgets[[i]])

      desc <- subset(controls$desc, mod %in% c(0, i))

      # Set the reactive environment of the modules. envs[[i]] is a reactive
      # value containing the module environment.
      moduleEnv <- reactive({
        input$.update

        for (j in seq_len(nrow(desc))) {
          if (.updateBtn) v <- isolate(input[[desc$inputId[j]]])
          else v <- input[[desc$inputId[j]]]
          assign(desc$name[j], v, envir = desc$env[[j]])
        }
        controls$env$ind[[i]]
      })

      # Update inputs and widget of the module
      observe({
        showHideControls(desc, session, moduleEnv())

        # Skip first evaluation, since widgets have already been rendered with
        # initial parameters
        if (get(".initial", envir = moduleEnv())) {
          assign(".initial", FALSE, envir = moduleEnv())
          assign(".session", session, envir = moduleEnv())
        } else {
          desc <<- updateControls(desc, session, moduleEnv())
          res <- eval(.expr, envir = moduleEnv())
          if (useCombineWidgets) res <- combineWidgets(res)
          if (is(res, "htmlwidget")) {
            output[[paste0("output", i)]] <- renderFunction(res)
          }
        }
      })
    }

    for (i in seq_len(controls$nmod)) {
      updateModule(i)
    }

    observeEvent(input$done, onDone(.expr, controls, .return, nrow, ncol))

    # save
    output$save <- downloadHandler(
      filename = function() {
        paste('mpWidget-', Sys.Date(), '.html', sep='')
      },
      content = function(con) {
        htmlwidgets::saveWidget(widget = onSave(.expr, controls, .return, nrow, ncol),
                                file = con, selfcontained = TRUE)
      }
    )
  }
}
