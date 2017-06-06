#' Dynamically show/hide controls in the UI
#'
#' @param .display expression that evaluates to a named list of boolean
#' @param desc subset of controls$desc containing only shared inputs and inputs
#'   for the current module
#' @param session shiny session
#' @param env module environment
#'
#' @noRd
showHideControls <- function(desc, session, env) {
  displayBool <- lapply(desc$display, eval, envir = env)
  for (i in seq_along(displayBool)) {
    if (is.logical(displayBool[[i]])) {
      shiny::updateCheckboxInput(
        session,
        inputId = paste0(desc$inputId[i], "_visible"),
        value = displayBool[[i]]
      )
    }
  }
}

#' Dynamically set input parameters like choices, minimal or maximal values, etc.
#'
#' @param .updateInputs expression that evaluate to a named list of lists
#' @inheritParams showHideControls
#'
#' @return data.frame 'desc' with updated column params
#' @noRd
updateControls <- function(desc, session, env) {

  for (i in seq_len(nrow(desc))) {
    newParams <- evalParams(desc$params[[i]], env)

    args <- list(session = session, inputId = desc$inputId[i])
    updateRequired <- FALSE

    for (p in setdiff(names(newParams), c("value", "label"))) {
      if (identical(newParams[[p]], desc$currentParams[[i]][[p]])) {
        next
      }

      updateRequired <- TRUE
      args[[p]] <- newParams[[p]]

      # Special case: update value of select input when choices are modified
      if (p == "choices" & desc$type[i] == "select") {
        actualSelection <- get(desc$name[i], envir = env)
        if (desc$multiple[[i]]) {
          args$selected <- intersect(actualSelection, newParams[[p]])
        } else {
          if (actualSelection %in% newParams[[p]]) {
            args$selected <- actualSelection
          }
        }
      }

      desc$currentParams[[i]][[p]] <-  newParams[[p]]
    }

    if (updateRequired) {
      updateInputFun <- getUpdateInputFun(desc$type[i])
      do.call(updateInputFun, args)
    }
  }

  desc
}

#' Private function that returns the function to use to update some type of inputs
#' @noRd
getUpdateInputFun <- function(type) {
  switch(
    type,
    slider = shiny::updateSliderInput,
    text = shiny::updateTextInput,
    numeric = shiny::updateNumericInput,
    password = shiny::updateTextInput,
    select = shiny::updateSelectInput,
    checkbox = shiny::updateCheckboxInput,
    radio = shiny::updateRadioButtons,
    date = shiny::updateDateInput,
    dateRange = shiny::updateDateRangeInput,
    checkboxGroup = shiny::updateCheckboxGroupInput
  )
}

#' Function called when user clicks on the "Done" button. It stops the shiny
#' gadget and returns the final htmlwidget
#'
#' @param .expr Expression that generates a htmlwidget
#' @param controls Object created with function preprocessControls
#'
#' @return a htmlwidget
#' @noRd
onDone <- function(.expr, controls, .return = function(w, e) {w}, nrow = NULL, ncol = NULL) {
  widgets <- lapply(controls$env$ind, function(e) {
    assign(".initial", TRUE, envir = e)
    assign(".session", NULL, envir = e)
    eval(.expr, envir = e)
  })

  shiny::stopApp(mwReturn(widgets, .return, controls$env$ind, nrow, ncol))
}

#' Function that takes a list of widgets and returns the first one if there is
#' only one or a combinedWidget with all widgets combined.
#'
#' @param widgets list of htmlwidgets
#'
#' @return a htmlwidget
#' @noRd
mwReturn <- function(widgets, .return, envs, nrow = NULL, ncol = NULL) {
  if (length(widgets) == 1) {
    finalWidget <- widgets[[1]]
  } else {
    finalWidget <- combineWidgets(list = widgets, nrow = nrow, ncol = ncol)
  }
  .return(finalWidget, envs)
}
