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
