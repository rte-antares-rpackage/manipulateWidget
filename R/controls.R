#Copyright © 2016 RTE Réseau de transport d’électricité

# Private function used to create input generator functions.
mwControlFactory <- function(type, inputFunction, params, valueVar = NULL) {

  res <- function(params) {
    if (!is.null(valueVar)) {
      if (length(valueVar) == 1) params[[valueVar]] <- params$value
      else {
        for (i in 1:length(valueVar)) params[[valueVar[i]]] <- params$value[[i]]
      }
      params$value <- NULL
    }
    if (is.null(params$label)) params$label <- params$inputId
    do.call(inputFunction, params)
  }

  attr(res, "params") <- params
  attr(res, "type") <- type
  res
}

#' Add a Slider to a manipulateWidget gadget
#'
#' @param min
#'   The minimum value that can be selected.
#' @param max
#'   The maximum value that can be selected.
#' @param value
#'   Initial value of the slider  A numeric vector of length one will create a
#'   regular slider; a numeric vector of length two will create a double-ended
#'   range slider
#' @param label
#'   Display label for the control. If \code{NULL}, the name of the corresponding
#'   variable is used.
#' @param ...
#'   Other arguments passed to function\code{\link[shiny]{sliderInput}}
#'
#' @return
#'   A function that will generate the input control.
#'
#' @examples
#'
#' if (require(plotly)) {
#'
#'   myWidget <- manipulateWidget(
#'     plot_ly(data.frame(x = 1:n, y = rnorm(n)), x=~x, y=~y, type = "scatter", mode = "markers"),
#'     n = mwSlider(1, 100, 10, label = "Number of values")
#'   )
#'
#'   Sys.sleep(0.5)
#'
#'   # Create a double ended slider to choose a range instead of a single value
#'   mydata <- data.frame(x = 1:100, y = rnorm(100))
#'
#'   manipulateWidget(
#'     plot_ly(mydata[n[1]:n[2], ], x=~x, y=~y, type = "scatter", mode = "markers"),
#'     n = mwSlider(1, 100, c(1, 10), label = "Number of values")
#'   )
#'
#' }
#'
#' @export
#' @family controls
mwSlider <- function(min, max, value, label = NULL, ...) {
  mwControlFactory(
    "slider",
    function(...) {tags$div(style = "padding:0 5px;", sliderInput(...))},
    list(min = min, max = max, value = value, label = label, ...)
  )
}

#' Add a text input to a manipulateWidget gadget
#'
#' @param value
#'   Initial value of the text input.
#' @param ...
#'   Other arguments passed to function\code{\link[shiny]{textInput}}
#' @inheritParams mwSlider
#'
#' @return
#' A function that will generate the input control.
#'
#' @examples
#' if (require(plotly)) {
#'   mydata <- data.frame(x = 1:100, y = rnorm(100))
#'   manipulateWidget({
#'       plot_ly(mydata, x=~x, y=~y, type = "scatter", mode = "markers") %>%
#'         layout(title = mytitle)
#'     },
#'     mytitle = mwText("Awesome title !")
#'   )
#' }
#'
#' @export
#' @family controls
mwText <- function(value = "", label = NULL, ...) {
  mwControlFactory(
    "text", textInput,
    list(value = value, label = label, ...)
  )
}

#' Add a numeric input to a manipulateWidget gadget
#'
#' @param value
#'   Initial value of the numeric input.
#' @param ...
#'   Other arguments passed to function\code{\link[shiny]{numericInput}}
#' @inheritParams mwSlider
#'
#' @return
#' A function that will generate the input control.
#'
#' @examples
#'
#' if (require(plotly)) {
#'   manipulateWidget({
#'       plot_ly(data.frame(x = 1:10, y = rnorm(10, mean, sd)), x=~x, y=~y,
#'               type = "scatter", mode = "markers")
#'     },
#'     mean = mwNumeric(0),
#'     sd = mwNumeric(1, min = 0, step = 0.1)
#'   )
#' }
#'
#' @export
#' @family controls
mwNumeric <- function(value, label = NULL, ...) {
  mwControlFactory(
    "numeric", numericInput,
    list(value = value, label = label, ...)
  )
}

#' Add a password to a manipulateWidget gadget
#'
#' @param value
#'   Default value of the input.
#' @param ...
#'   Other arguments passed to function\code{\link[shiny]{passwordInput}}
#' @inheritParams mwSlider
#'
#' @return
#' A function that will generate the input control.
#'
#' @examples
#' if (require(plotly)) {
#'   manipulateWidget(
#'     {
#'       if (passwd != 'abc123') {
#'         plot_ly(type = "scatter", mode="markers") %>%
#'           layout(title = "Wrong password. True password is 'abc123'")
#'       } else {
#'         plot_ly(data.frame(x = 1:10, y = rnorm(10)), x=~x, y=~y, type = "scatter", mode = "markers")
#'       }
#'     },
#'     user = mwText(label = "Username"),
#'     passwd = mwPassword(label = "Password")
#'   )
#' }
#'
#' @export
#' @family controls
mwPassword <- function(value = "", label = NULL, ...) {
  mwControlFactory(
    "password", passwordInput,
    list(value = value, label = label, ...)
  )
}

#' Add a Select list input to a manipulateWidget gadget
#'
#' @param choices
#'   Vector or list of choices. If it is named, then the names rather than the
#'   values are displayed to the user.
#' @param value
#'   Initial value of the input. If not specified, the first choice is used.
#' @param ...
#'   Other arguments passed to function\code{\link[shiny]{selectInput}}.
#' @param multiple
#'   Is selection of multiple items allowed?
#' @inheritParams mwSlider
#'
#' @return
#' A function that will generate the input control.
#'
#' @examples
#' if (require(plotly)) {
#'   mydata <- data.frame(x = 1:100, y = rnorm(100))
#'
#'   manipulateWidget(
#'     {
#'       mode <- switch(type, points = "markers", lines = "lines", both = "markers+lines")
#'       plot_ly(mydata, x=~x, y=~y, type = "scatter", mode = mode)
#'     },
#'     type = mwSelect(c("points", "lines", "both"))
#'   )
#'
#'   Sys.sleep(0.5)
#'
#'   # Select multiple values
#'   manipulateWidget(
#'     {
#'       if (length(species) == 0) mydata <- iris
#'       else mydata <- iris[iris$Species %in% species,]
#'
#'       plot_ly(mydata, x = ~Sepal.Length, y = ~Sepal.Width,
#'               color = ~droplevels(Species), type = "scatter", mode = "markers")
#'     },
#'     species = mwSelect(levels(iris$Species), multiple = TRUE)
#'   )
#' }
#'
#' @export
#' @family controls
mwSelect <- function(choices = value, value = NULL, label = NULL, ..., multiple = FALSE) {
  mwControlFactory(
    "select", selectizeInput,
    list(choices = choices, value = value, label = label, ..., multiple = multiple),
    valueVar = "selected"
  )
}

#' Add a checkbox to a manipulateWidget gadget
#'
#' @param value
#'   Initial value of the input.
#' @param ...
#'   Other arguments passed to function\code{\link[shiny]{checkboxInput}}
#' @inheritParams mwSlider
#'
#' @return
#' A function that will generate the input control.
#'
#' @examples
#'
#' if(require(plotly)) {
#'  manipulateWidget(
#'    {
#'        plot_ly(iris, x = ~Sepal.Length, y = ~Sepal.Width,
#'                color = ~Species, type = "scatter", mode = "markers") %>%
#'          layout(showlegend = legend)
#'    },
#'    legend = mwCheckbox(TRUE, "Show legend")
#'  )
#' }
#'
#' @export
#' @family controls
mwCheckbox <- function(value = FALSE, label = NULL, ...) {
  mwControlFactory(
    "checkbox", checkboxInput,
    list(value = value, label = label, ...)
  )
}

#' Add radio buttons to a manipulateWidget gadget
#'
#' @param choices
#'   Vector or list of choices. If it is named, then the names rather than the
#'   values are displayed to the user.
#' @param value
#'   Initial value of the input. If not specified, the first choice is used.
#' @param ...
#'   Other arguments passed to function\code{\link[shiny]{radioButtons}}
#' @inheritParams mwSlider
#'
#' @return
#' A function that will generate the input control.
#'
#' @examples
#' if (require(plotly)) {
#'   mydata <- data.frame(x = 1:100, y = rnorm(100))
#'
#'   manipulateWidget(
#'     {
#'       mode <- switch(type, points = "markers", lines = "lines", both = "markers+lines")
#'       plot_ly(mydata, x=~x, y=~y, type = "scatter", mode = mode)
#'     },
#'     type = mwRadio(c("points", "lines", "both"))
#'   )
#' }
#'
#' @export
#' @family controls
mwRadio <- function(choices, value = NULL, label = NULL, ...) {
  mwControlFactory(
    "radio", radioButtons,
    list(choices = choices, value = value, label = label, ...),
    valueVar = "selected"
  )
}

#' Add a date picker to a manipulateWidget gadget
#'
#' @param value
#'   Default value of the input.
#' @param ...
#'   Other arguments passed to function\code{\link[shiny]{dateInput}}
#' @inheritParams mwSlider
#'
#' @return
#' A function that will generate the input control.
#'
#' @examples
#' if (require(dygraphs) && require(xts)) {
#'   mydata <- xts(rnorm(365), order.by = as.Date("2017-01-01") + 0:364)
#'
#'   manipulateWidget(
#'     dygraph(mydata) %>% dyEvent(date, "Your birthday"),
#'     date = mwDate("2017-03-27", label = "Your birthday date",
#'                   min = "2017-01-01", max = "2017-12-31")
#'   )
#' }
#'
#' @export
#' @family controls
mwDate <- function(value = NULL, label = NULL, ...) {
  mwControlFactory(
    "date", dateInput,
    list(value = value, label = label, ...)
  )
}

#' Add a date range picker to a manipulateWidget gadget
#'
#' @param value
#'   Vector containing two dates (either Date objects pr a string in yyy-mm-dd
#'   format) representing the initial date range selected.
#' @param ...
#'   Other arguments passed to function\code{\link[shiny]{dateRangeInput}}
#' @inheritParams mwSlider
#'
#' @return
#' A function that will generate the input control.
#'
#' @examples
#' if (require(dygraphs) && require(xts)) {
#'   mydata <- xts(rnorm(365), order.by = as.Date("2017-01-01") + 0:364)
#'
#'   manipulateWidget(
#'     dygraph(mydata) %>% dyShading(from=period[1], to = period[2], color = "#CCEBD6"),
#'     period = mwDateRange(c("2017-03-01", "2017-04-01"),
#'                   min = "2017-01-01", max = "2017-12-31")
#'   )
#' }
#'
#' @export
#' @family controls
mwDateRange <- function(value = c(Sys.Date(), Sys.Date() + 1), label = NULL, ...) {
  mwControlFactory(
    "dateRange", dateRangeInput,
    list(value = value, label = label, ...),
    valueVar = c("start", "end")
  )
}

#' Add a group of checkboxes to a manipulateWidget gadget
#'
#' @param choices
#'   Vector or list of choices. If it is named, then the names rather than the
#'   values are displayed to the user.
#' @param value
#'   Vector containing the values initially selected
#' @param ...
#'   Other arguments passed to function\code{\link[shiny]{checkboxGroupInput}}
#' @inheritParams mwSlider
#'
#' @return
#' A function that will generate the input control.
#'
#' @examples
#' if (require(plotly)) {
#'   manipulateWidget(
#'     {
#'       if (length(species) == 0) mydata <- iris
#'       else mydata <- iris[iris$Species %in% species,]
#'
#'       plot_ly(mydata, x = ~Sepal.Length, y = ~Sepal.Width,
#'               color = ~droplevels(Species), type = "scatter", mode = "markers")
#'     },
#'     species = mwCheckboxGroup(levels(iris$Species))
#'   )
#' }
#'
#' @export
#' @family controls
mwCheckboxGroup <- function(choices, value = c(), label = NULL, ...) {
  mwControlFactory(
    "checkboxGroup", checkboxGroupInput,
    list(choices = choices, value = value, label = label, ...),
    valueVar = "selected"
  )
}

