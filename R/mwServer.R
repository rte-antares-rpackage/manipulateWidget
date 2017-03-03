mwServer <- function(.expr, controls, widgets,
                     renderFunction,
                     .display, .updateInputs, .compareLayout,
                     .updateBtn) {

  function(input, output, session) {
    # Since the widget has already been created with the initial values, we want
    # to skip the first evaluation of the widget by the server function. This is
    # why we create the following variable.
    firstEval <- TRUE

    # Ensure that initial values of select inputs with multiple = TRUE are in
    # same order than the user asked.
    selectInputList <- subset(controls$inputs, type == "select" & multiple)$name
    for (v in selectInputList) {
      shiny::updateSelectInput(session, v, selected = initValues[[v]])
    }

    updateModule <- function(i) {
      print(i)
      # Initialize the widgets with their first evaluation
      output[[paste0("output", i)]] <- renderFunction(widgets[[i]])

      desc <- subset(controls$inputs, mod %in% c(0, i))

      # Set the reactive environment of the modules. envs[[i]] is a reactive
      # value containing the module environment.
      moduleEnv <- reactive({
        input$.update

        for (j in seq_len(nrow(desc))) {
          if (.updateBtn) v <- eval(parse(text = sprintf("isolate(input$%s)", desc$inputId[j])))
          else v <- eval(parse(text = sprintf("input$%s", desc$inputId[j])))
          assign(desc$name[j], v, envir = desc$env[[j]])
        }
        controls$env$ind[[i]]
      })

      # Update inputs and widget of the module
      observe({
        print(paste("Updating module", i))

        # Show/hide controls
        displayBool <- eval(.display, envir = moduleEnv())
        if (length(displayBool) > 0) {
          for (n in names(displayBool)) {
            inputDesc <- subset(desc, name == n)
            if (nrow(inputDesc) == 1) {
              shiny::updateCheckboxInput(
                session,
                inputId = paste0(inputDesc$inputId, "_visible"),
                value = displayBool[[n]])
            }
          }
        }

        # Update widgets
        res <- eval(.expr, envir = moduleEnv())
        if (is(res, "htmlwidget")) {
          output[[paste0("output", i)]] <- renderFunction(res)
        }
      })
    }

    for (i in seq_len(controls$nmod)) {
      updateModule(i)
    }

    # Update inputs

    # observe({
    #   inputEnv <- getInputEnv(inputList(), session, "output", 1, .env)
    #   controlDesc <<- updateInputs(session, input, controlDesc, .display,
    #                                .compare, .updateInputs, inputEnv, suffix = "")
    #   if (firstEval) {
    #     firstEval <<- FALSE
    #   } else {
    #     outputWidget(.expr, output, renderFunction, inputEnv)
    #   }
    # })
    #
    # if (compareMode) {
    #   # Initialize the widget with its first evaluation
    #   output$output2 <- renderFunction(initWidget2)
    #
    #   inputList2 <- reactive({
    #     input$.update
    #
    #     res <- lapply(controlDesc2$name, function(s) {
    #       if (.updateBtn) eval(parse(text = sprintf("isolate(input$%s)", s)))
    #       else eval(parse(text = sprintf("input$%s", s)))
    #     })
    #     names(res) <- controlDesc$name
    #
    #     res
    #   })
    #
    #   # Ensure that initial values of select inputs with multiple = TRUE are in
    #   # same order than the user asked.
    #   for (v in selectInputList) {
    #     inputId <- paste0(v, "2")
    #     shiny::updateSelectInput(session, inputId, selected = initValues2[[v]])
    #   }
    #
    #   observe({
    #     inputEnv <- getInputEnv(inputList2(), session, "output2", 2, .env)
    #     controlDesc2 <<- updateInputs(session, input, controlDesc2, .display,
    #                                   .compare, .updateInputs, inputEnv, suffix = "2")
    #     if (firstEval2) {
    #       firstEval2 <<- FALSE
    #     } else {
    #       outputWidget(.expr, output, renderFunction, inputEnv)
    #     }
    #   })
    # }
    #
    observeEvent(input$done, {
      widgets <- lapply(controls$env$ind, function(e) {
        eval(.expr, envir = e)
      })
      if (length(widgets) == 1) shiny::stopApp(widgets[[1]])
      else {
        shiny::stopApp(
          combineWidgets(list=widgets)
        )
      }
    })
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
      shiny::updateCheckboxInput(session, inputId = paste0(id, "_visible"),
                                 value = .displayBool[[id]])
    }
  }

  #Update choices of select inputs if parameter .choices is set
  newParams <- eval(.updateInputs, envir = env)

  for (n in names(newParams)) {
    # TODO: in comparison mode, common inputs are updated twice.
    if (n %in% names(.compare)) inputId <- paste0(n, suffix)
    else inputId <- n
    desc <- controlDesc[controlDesc$name == inputId,]
    updateInputFun <- switch(
      desc$type,
      slider = shiny::updateSliderInput,
      text = shiny::updateTextInput,
      numeric = shiny::updateNumericInput,
      password = shiny::updateTextInput,
      select = shiny::updateSelectizeInput,
      checkbox = shiny::updateCheckboxInput,
      radio = shiny::updateRadioButtons,
      date = shiny::updateDateInput,
      dateRange = shiny::updateDateRangeInput,
      checkboxGroup = shiny::updateCheckboxGroupInput
    )

    for (p in names(newParams[[n]])) {
      if (identical(newParams[[n]][[p]], desc$params[[1]][[p]])) {
        next
      }
      args <- newParams[[n]][p]
      args$session <- session
      args$inputId <- inputId

      # Special case: update value of select input when choices are modified
      if (p == "choices" & desc$type == "select") {
        if (desc$multiple) {
          args$selected <- intersect(env[[n]], newParams[[n]][[p]])
        } else {
          if (env[[n]] %in% newParams[[n]][[p]]) {
            args$selected <- env[[n]]
          }
        }
      }
      do.call(updateInputFun, args)

      controlDesc$params[controlDesc$name == inputId][[1]][[p]] <-  newParams[[n]][[p]]
    }
  }

  return(controlDesc)
}
