mwSlider <- function(min, max, value, label = NULL, ...) {
  function(id) {
    if (is.null(label)) label <- id
    sliderInput(id, label, min, max, value, ...)
  }
}

mwText <- function(value = "", label = NULL, ...) {
  function(id) {
    if (is.null(label)) label <- id
    textInput(id, label, value, ...)
  }
}

mwNumeric <- function(value, label = NULL, ...) {
  function(id) {
    if (is.null(label)) label <- id
    numericInput(id, label, value, ...)
  }
}

mwPassword <- function(value = "", label = NULL, ...) {
  function(id) {
    if (is.null(label)) label <- id
    passwordInput(id, label, value, ...)
  }
}

mwSelect <- function(choices, value = NULL, label = NULL, ...) {
  function(id) {
    if (is.null(label)) label <- id
    selectInput(id, label, choices, value, ...)
  }
}

mwCheckbox <- function(value = FALSE, label = NULL, ...) {
  function(id) {
    if (is.null(label)) label <- id
    checkboxInput(id, label, value, ...)
  }
}

mwRadio <- function(choices, value = NULL, label = NULL, ...) {
  function(id) {
    if (is.null(label)) label <- id
    radioButtons(id, label, choices, value, ...)
  }
}

mwDate <- function(value = NULL, label = NULL, ...) {
  function(id) {
    if (is.null(label)) label <- id
    dateInput(id, label, value, ...)
  }
}

mwDateRange <- function(start = NULL, end = NULL, label = NULL, ...) {
  function(id) {
    if (is.null(label)) label <- id
    dateRangeInput(id, label, start, end, ...)
  }
}



