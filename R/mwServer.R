mwServer <- function(.expr, initWidget, initWidget2 = NULL,
                     initValues, initValues2 = NULL,
                     renderFunction,
                     controlDesc, .display, .updateInputs, .compare, .compareLayout,
                     .updateBtn,
                     .env) {

  function(input, output, session) {
    compareMode <- !is.null(initWidget2)
    selectInputList <- controlDesc[controlDesc$type == "select" & controlDesc$multiple, "name"]

    # Since the widget has already been created with the initial values, we want
    # to skip the first evaluation of the widget by the server function. This is
    # why we create the following variable.
    firstEval <- TRUE

    if (compareMode) {
      controlDesc2 <- controlDesc
      controlDesc2$name <- ifelse(
        controlDesc2$name %in% names(.compare),
        paste0(controlDesc2$name, "2"),
        controlDesc2$name
      )
      selectInputList2 <- controlDesc2[controlDesc2$type == "select" & controlDesc2$multiple, "name"]
      firstEval2 <- TRUE
    }

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
      inputEnv <- getInputEnv(inputList(), session, "output", 1, .env)
      if (firstEval) {
        firstEval <<- FALSE
      } else {
        controlDesc <<- updateInputs(session, input, controlDesc, .display,
                                     .compare, .updateInputs, inputEnv, suffix = "")
        outputWidget(.expr, output, renderFunction, inputEnv)
      }
    })

    if (compareMode) {
      # Initialize the widget with its first evaluation
      output$output2 <- renderFunction(initWidget2)

      inputList2 <- reactive({
        input$.update

        res <- lapply(controlDesc2$name, function(s) {
          if (.updateBtn) eval(parse(text = sprintf("isolate(input$%s)", s)))
          else eval(parse(text = sprintf("input$%s", s)))
        })
        names(res) <- controlDesc$name

        res
      })

      # Ensure that initial values of select inputs with multiple = TRUE are in
      # same order than the user asked.
      for (v in selectInputList) {
        inputId <- paste0(v, "2")
        shiny::updateSelectInput(session, inputId, selected = initValues2[[v]])
      }

      observe({
        inputEnv <- getInputEnv(inputList2(), session, "output2", 2, .env)
        if (firstEval2) {
          firstEval2 <<- FALSE
        } else {
          controlDesc2 <<- updateInputs(session, input, controlDesc2, .display,
                                        .compare, .updateInputs, inputEnv, suffix = "2")
          outputWidget(.expr, output, renderFunction, inputEnv)
        }
      })
    }

    observeEvent(input$done, {
      inputEnv <- getInputEnv(inputList(), NULL, output, 1, .env, TRUE)

      if (!compareMode) {
        stopApp(eval(.expr, envir = inputEnv))
      } else {
        inputEnv2 <- getInputEnv(inputList2(), NULL, output, 2, .env, TRUE)

        stopApp(combineWidgets(
          ncol = ifelse(.compareLayout == "v", 1, 2),
          eval(.expr, envir = inputEnv),
          eval(.expr, envir = inputEnv2)
        ))
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

updateInputs <- function(session, input, controlDesc, .display, .compare, .updateInputs, env, suffix = "") {
  # Set visibility of inputs when parameter .display is set
  .displayBool <- eval(.display, envir = env)
  if (length(.displayBool) > 0) {
    names(.displayBool) <- ifelse(
      names(.displayBool) %in% names(.compare),
      paste0(names(.displayBool), suffix),
      names(.displayBool)
    )

    for (id in names(.displayBool)) {
      updateCheckboxInput(session, inputId = paste0(id, "_visible"),
                          value = .displayBool[[id]])
    }
  }

  #Update choices of select inputs if parameter .choices is set
  newParams <- eval(.updateInputs, envir = env)

  for (n in names(newParams)) {
    inputId <- paste0(n, suffix)
    desc <- controlDesc[controlDesc$name == inputId,]
    updateInputFun <- switch(
      desc$type,
      slider = updateSliderInput,
      text = updateTextInput,
      numeric = updateNumericInput,
      password = updatePasswordInput,
      select = updateSelectizeInput,
      checkbox = updateCheckboxInput,
      radio = updateRadioButtons,
      date = updateDateInput,
      dateRange = updateDateRangeInput,
      checkboxGroup = updateCheckboxGroupInput
    )

    for (p in names(newParams[[n]])) {
      if (identical(newParams[[n]][[p]], desc$params[[1]][[p]])) {
        next
      }
      args <- newParams[[n]][p]
      args$session <- session
      args$inputId <- inputId
      do.call(updateInputFun, args)

      controlDesc$params[controlDesc$name == inputId][[1]][[p]] <-  newParams[[n]][[p]]
    }
  }

  return(controlDesc)
}
