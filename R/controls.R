#' Add a Slider to a manipulateWidget gadget
#' @export
#' @family controls
mwSlider <- function(min, max, value, label = NULL, ...) {
  function(id) {
    if (is.null(label)) label <- id
    sliderInput(id, label, min, max, value, ...)
  }
}

#' Add a text input to a manipulateWidget gadget
#' @export
#' @family controls
mwText <- function(value = "", label = NULL, ...) {
  function(id) {
    if (is.null(label)) label <- id
    textInput(id, label, value, ...)
  }
}

#' Add a numeric input to a manipulateWidget gadget
#' @export
#' @family controls
mwNumeric <- function(value, label = NULL, ...) {
  function(id) {
    if (is.null(label)) label <- id
    numericInput(id, label, value, ...)
  }
}

#' Add a password to a manipulateWidget gadget
#' @export
#' @family controls
mwPassword <- function(value = "", label = NULL, ...) {
  function(id) {
    if (is.null(label)) label <- id
    passwordInput(id, label, value, ...)
  }
}

#' Add a Select list input to a manipulateWidget gadget
#' @export
#' @family controls
mwSelect <- function(choices, value = NULL, label = NULL, ...) {
  function(id) {
    if (is.null(label)) label <- id
    selectInput(id, label, choices, value, ...)
  }
}

#' Add a checkbox to a manipulateWidget gadget
#' @export
#' @family controls
mwCheckbox <- function(value = FALSE, label = NULL, ...) {
  function(id) {
    if (is.null(label)) label <- id
    checkboxInput(id, label, value, ...)
  }
}

#' Add radio buttons to a manipulateWidget gadget
#' @export
#' @family controls
mwRadio <- function(choices, value = NULL, label = NULL, ...) {
  function(id) {
    if (is.null(label)) label <- id
    radioButtons(id, label, choices, value, ...)
  }
}

#' Add a date picker to a manipulateWidget gadget
#' @export
#' @family controls
mwDate <- function(value = NULL, label = NULL, ...) {
  function(id) {
    if (is.null(label)) label <- id
    dateInput(id, label, value, ...)
  }
}

#' Add a date range picker to a manipulateWidget gadget
#' @export
#' @family controls
mwDateRange <- function(start = NULL, end = NULL, label = NULL, ...) {
  function(id) {
    if (is.null(label)) label <- id
    dateRangeInput(id, label, start, end, ...)
  }
}

#' Add a group of checkboxes to a manipulateWidget gadget
#' @export
#' @family controls
mwCheckboxGroup <- function(choices, value = NULL, label = NULL, ...) {
  function(id) {
    if (is.null(label)) label <- id
    checkboxGroupInput(id, label, choices, value, ...)
  }
}

