mwServer <- function(.expr, initWidget, initWidget2 = NULL,
                     initValues, initValues2 = NULL,
                     renderFunction,
                     controlDesc, .display, .choices, .compare, .compareLayout,
                     .updateBtn,
                     .env) {

  compareMode <- !is.null(initWidget2)
  selectInputList <- controlDesc$name[controlDesc$type == "select"]

  if (compareMode) {
    controlDesc2 <- controlDesc
    controlDesc2$name <- ifelse(
      controlDesc2$name %in% names(.compare),
      paste0(controlDesc2$name, "2"),
      controlDesc2$name
    )
    selectInputList2 <- controlDesc2$name[controlDesc$type == "select"]
  }

  function(input, output, session) {
    # Initialize the widget with its first evaluation
    output$output <- renderFunction(initWidget)

    # Ensure that initial values of select inputs with multiple = TRUE are in
    # same order than the user asked.
    for (v in selectInputList) {
      shiny::updateSelectInput(session, v, selected = initValues[[v]])
    }

    inputList <- reactive({
      input$.update

      res <- lapply(controlDesc$name, function(s) {
        if (.updateBtn) eval(parse(text = sprintf("isolate(input$%s)", s)))
        else eval(parse(text = sprintf("input$%s", s)))
      })
      names(res) <- controlDesc$name

      res
    })

    observe({
      # get input current values
      inputValues <- inputList()
      inputEnv <- getInputEnv(inputValues, session, "output", 1, .env)

      controlDesc <<- updateInputs(session, input, controlDesc, .display, .choices, inputEnv, suffix = "")

      # Output or update the widget
      outputWidget(.expr, output, renderFunction, inputEnv)
    })

    if (compareMode) {
      # Initialize the widget with its first evaluation
      output$output2 <- renderFunction(initWidget2)

      # Ensure that initial values of select inputs with multiple = TRUE are in
      # same order than the user asked.
      for (v in selectInputList) {
        shiny::updateSelectInput(session, v, selected = initValues[[v]])
      }

      inputList2 <- reactive({
        input$.update

        res <- lapply(controlDesc2$name, function(s) {
          if (.updateBtn) eval(parse(text = sprintf("isolate(input$%s)", s)))
          else eval(parse(text = sprintf("input$%s", s)))
        })
        names(res) <- controlDesc$name

        res
      })

      observe({
        # get input current values
        # get input current values
        inputValues <- inputList2()
        inputEnv <- getInputEnv(inputValues, session, "output2", 2, .env)

        # Update the interface if parameter .display is set
        .displayBool <- eval(.display, envir = inputEnv)
        if (!is.null(.displayBool)) {
          names(.displayBool)[names(.displayBool) %in% names(.compare)] <- paste0(
            names(.displayBool)[names(.displayBool) %in% names(.compare)], "2"
          )
        }

        for (id in names(.displayBool)) {
          updateCheckboxInput(session, inputId = paste0(id, "_visible"),
                              value = .displayBool[[id]])
        }

        # Output or update the widget
        outputWidget(.expr, output, renderFunction, inputEnv)
      })
    }

    observeEvent(input$done, {
      inputValues <- inputList()
      inputValues$.initial <- TRUE
      inputValues$.session <- NULL
      inputValues$.id <- 1
      inputEnv <- list2env(inputValues, parent = .env)

      if (compareMode) {
        inputValues2 <- inputList2()
        inputValues2$.initial <- TRUE
        inputValues2$.session <- NULL
        inputValues2$.id <- 2
        inputEnv2 <- list2env(inputValues2, parent = .env)

        stopApp(combineWidgets(
          ncol = ifelse(.compareLayout == "v", 1, 2),
          eval(.expr, envir = inputEnv),
          eval(.expr, envir = inputEnv2)
        ))
      } else {
        stopApp(eval(.expr, envir = inputEnv))
      }
    })
  }
}

getInputEnv <- function(inputValues, session, output, id, env, initial = FALSE) {
  inputValues$.initial <- initial
  inputValues$.session <- session
  inputValues$.output <- output
  inputValues$.id <- id

  list2env(inputValues, parent = env)
}

outputWidget <- function(.expr, output, renderFunction, env) {
  res <- eval(.expr, envir = env)
  if (is(res, "htmlwidget")) {
    output[[env$.output]] <- renderFunction(res)
  }
}

updateInputs <- function(session, input, controlDesc, .display, .choices, env, suffix = "") {
  # Update the interface if parameter .display is set
  .displayBool <- eval(.display, envir = env)
  if (length(.displayBool) == 0) return(controlDesc)

  names(.displayBool) <- ifelse(
    names(.displayBool) %in% names(.choices),
    paste0(names(.displayBool), suffix),
    names(.displayBool)
  )

  for (id in names(.displayBool)) {
    updateCheckboxInput(session, inputId = paste0(id, "_visible"),
                        value = .displayBool[[id]])
  }

  #Update choices if parameter .choices is set (and modifies some input)
  newChoices <- eval(.choices, envir = env)
  names(newChoices) <- ifelse(
    names(newChoices) %in% names(.choices),
    paste0(names(newChoices), suffix),
    names(newChoices)
  )

  for (id in names(newChoices)) {
    possibleChoices <- unlist(newChoices[[id]])
    desc <- controlDesc[controlDesc$name == id,]

    if (identical(newChoices[[id]], desc$choices[[1]])) {
      next
    }

    if (desc$multiple) {
      newValue <- intersect(env[[id]], possibleChoices)
    } else {
      if (env[[id]] %in% possibleChoices) {
        newValue <- env[[id]]
      } else {
        newValue <- possibleChoices[1]
      }
    }

    updateSelectInput(session, id, choices = newChoices[[id]],
                      selected = newValue)

    controlDesc$choices[controlDesc$name == id] <- list(newChoices[[id]])
  }

  return(controlDesc)
}
