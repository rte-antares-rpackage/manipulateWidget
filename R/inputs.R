#' Private function that converts ... in a list of expressions. This is
#' similar to "substitute" but for the dots argument.
#' @noRd
dotsToExpr <- function() {
  eval(substitute(alist(...), parent.frame()))
}

#' Private function that generates functions that generate HTML corresponding
#' to a shiny input.
#'
#' @param func shiny function that generate the HTML of an input
#' @param valueArgName name of the parameter of 'func' corresponding to the
#'   value of the input.
#'
#' @return
#' A function that takes arguments id, label, value, params and returns
#' shiny tag.
#' @noRd
htmlFuncFactory <- function(func, valueArgName = "value") {
  function(id, label, value, params, ns = NULL) {
    params$inputId <- id
    params$label <- label
    params[valueArgName] <- list(value)
    do.call(func, params)
  }
}

changeValueParam <- function(func, valueArgName) {
  function(...) {
    params <- list(...)
    if ("value" %in% names(params)) {
      params[[valueArgName]] <- params$value
      params$value <- NULL
    }
    do.call(shiny::updateSelectInput, params)
  }
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
#' @param .display expression that evaluates to TRUE or FALSE, indicating when
#'   the input control should be shown/hidden.
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
mwSlider <- function(min, max, value, label = NULL, ..., .display = TRUE) {
  params <- dotsToExpr()
  params$min <- substitute(min)
  params$max <- substitute(max)
  value <- substitute(value)
  Input(
    type = "slider", value = value, label = label, params = params,
    display = substitute(.display),
    validFunc = function(x, params) {
      if (is.null(x) || is.na(x)) return(c(params$min, params$max))
      pmin(pmax(params$min, x, na.rm = TRUE), params$max, na.rm = TRUE)
    },
    htmlFunc = htmlFuncFactory(function(...) {
      tags$div(style = "padding:0 5px;", shiny::sliderInput(...))
    }),
    htmlUpdateFunc = shiny::updateSliderInput
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
mwText <- function(value = "", label = NULL, ..., .display = TRUE) {
  params <- dotsToExpr()
  value <- substitute(value)
  Input(
    type = "text", value = value, label = label, params = params,
    display = substitute(.display),
    validFunc = function(x, params) {
      if(length(x) == 0) return("")
      as.character(x)[1]
    },
    htmlFunc = htmlFuncFactory(shiny::textInput),
    htmlUpdateFunc = shiny::updateTextInput
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
mwNumeric <- function(value, label = NULL, ..., .display = TRUE) {
  params <- dotsToExpr()
  value <- substitute(value)
  Input(
    type = "numeric", value = value, label = label, params = params,
    display = substitute(.display),
    validFunc = function(x, params) {
      if (is.null(x) || !is.numeric(x)) return(NULL)
      min(max(params$min, x), params$max)
    },
    htmlFunc = htmlFuncFactory(shiny::numericInput),
    htmlUpdateFunc = shiny::updateNumericInput
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
mwPassword <- function(value = "", label = NULL, ..., .display = TRUE) {
  params <- dotsToExpr()
  value <- substitute(value)
  Input(
    type = "password", value = value, label = label, params = params,
    display = substitute(.display),
    validFunc = function(x, params) {
      if(length(x) == 0) return("")
      as.character(x)[1]
    },
    htmlFunc = htmlFuncFactory(shiny::passwordInput),
    htmlUpdateFunc = shiny::updateTextInput
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
mwSelect <- function(choices = value, value = NULL, label = NULL, ...,
                      multiple = FALSE, .display = TRUE) {
  params <- dotsToExpr()
  params$choices <- substitute(choices)
  params$multiple <- substitute(multiple)
  value <- substitute(value)
  Input(
    type = "select", value = value, label = label, params = params,
    display = substitute(.display),
    validFunc = function(x, params) {
      x <- intersect(x, unlist(params$choices))
      if (params$multiple) return(x)
      else if (length(x) > 0) return(x[1])
      else return(params$choices[[1]])
    },
    htmlFunc = htmlFuncFactory(shiny::selectInput, "selected"),
    htmlUpdateFunc = changeValueParam(shiny::updateSelectInput, "selected")
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
mwCheckbox <- function(value = FALSE, label = NULL, ..., .display = TRUE) {
  params <- dotsToExpr()
  value <- substitute(value)
  Input(
    type = "checkbox", value = value, label = label, params = params,
    display = substitute(.display),
    validFunc = function(x, params) {
      if (is.null(x)) return(FALSE)
      x <- as.logical(x)
      if (is.na(x)) x <- FALSE
      x
    },
    htmlFunc = htmlFuncFactory(shiny::checkboxInput),
    htmlUpdateFunc = shiny::updateCheckboxInput
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
mwRadio <- function(choices, value = NULL, label = NULL, ..., .display = TRUE) {
  params <- dotsToExpr()
  params$choices <- substitute(choices)
  value <- substitute(value)
  Input(
    type = "radio", value = value, label = label, params = params,
    display = substitute(.display),
    validFunc = function(x, params) {
      if (length(params$choices) == 0) return(NULL)
      if (is.null(x) || !x %in% unlist(params$choices)) return(params$choices[[1]])
      x
    },
    htmlFunc = htmlFuncFactory(shiny::radioButtons, valueArgName = "selected"),
    htmlUpdateFunc = changeValueParam(shiny::updateRadioButtons, "selected")
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
mwDate <- function(value = NULL, label = NULL, ..., .display = TRUE) {
  params <- dotsToExpr()
  value <- substitute(value)
  Input(
    type = "date", value = value, label = label, params = params,
    display = substitute(.display),
    validFunc = function(x, params) {
      if (length(x) == 0) x <- Sys.Date()
      x <- as.Date(x)
      if (!is.null(params$min)) params$min <- as.Date(params$min)
      if (!is.null(params$max)) params$max <- as.Date(params$max)
      x <- min(max(x, params$min), params$max)
    },
    htmlFunc = htmlFuncFactory(shiny::dateInput),
    htmlUpdateFunc = shiny::updateDateInput
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
#' An Input object
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
mwDateRange <- function(value = c(Sys.Date(), Sys.Date() + 1), label = NULL, ...,
                        .display = TRUE) {

  params <- dotsToExpr()
  value <- substitute(value)
  Input(
    type = "dateRange", value = value, label = label, params = params,
    display = substitute(.display),
    validFunc = function(x, params) {
      if (length(x) == 0) x <- c(Sys.Date(), Sys.Date())
      else if (length(x) == 1) x <-  c(x, Sys.Date())
      x <- as.Date(x)
      x[is.na(x)] <- Sys.Date()
      if (!is.null(params$min)) {
        params$min <- as.Date(params$min)
        if(x[1] == Sys.Date()){
          x[1] <- params$min
        }
      }
      if (!is.null(params$max)) {
        params$max <- as.Date(params$max)
        if(x[2] == Sys.Date()){
          x[2] <- params$max
        }
      }
      x <- sapply(x, function(d) min(max(d, params$min), params$max))
      as.Date(x, origin = "1970-01-01")
    },
    htmlFunc = function(id, label, value, params, ns) {
      params$inputId <- id
      params$label <- label
      params$start <- value[[1]]
      params$end <- value[[2]]
      do.call(shiny::dateRangeInput, params)
    },
    htmlUpdateFunc = function(...) {
      params <- list(...)
      if ("value" %in% names(params)) {
        params$start <- params$value[[1]]
        params$end <- params$value[[2]]
        params$value <- NULL
      }
      do.call(shiny::updateDateRangeInput, params)
    }
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
mwCheckboxGroup <- function(choices, value = c(), label = NULL, ..., .display = TRUE) {
  params <- dotsToExpr()
  params$choices <- substitute(choices)
  value <- substitute(value)
  Input(
    type = "checkboxGroup", value = value, label = label, params = params,
    display = substitute(.display),
    validFunc = function(x, params) {
      intersect(x, unlist(params$choices))
    },
    htmlFunc = htmlFuncFactory(shiny::checkboxGroupInput, "selected"),
    htmlUpdateFunc = changeValueParam(shiny::updateCheckboxGroupInput, "selected")
  )
}

#' Shared Value
#'
#' This function creates a virtual input that can be used to store a dynamic
#' shared variable that is accessible in inputs as well as in output.
#'
#' @param expr Expression used to compute the value of the input.
#'
#' @return An Input object of type "sharedValue".
#'
#' @examples
#'
#' if (require(plotly)) {
#'   # Plot the characteristics of a car and compare with the average values for
#'   # cars with same number of cylinders.
#'   # The shared variable 'subsetCars' is used to avoid subsetting multiple times
#'   # the data: this value is updated only when input 'cylinders' changes.
#'   colMax <- apply(mtcars, 2, max)
#'
#'   plotCar <- function(cardata, carName) {
#'     carValues <- unlist(cardata[carName, ])
#'     carValuesRel <- carValues / colMax
#'
#'     avgValues <- round(colMeans(cardata), 2)
#'     avgValuesRel <- avgValues / colMax
#'
#'     plot_ly() %>%
#'       add_bars(x = names(cardata), y = carValuesRel, text = carValues,
#'                hoverinfo = c("x+text"), name = carName) %>%
#'       add_bars(x = names(cardata), y = avgValuesRel, text = avgValues,
#'                hoverinfo = c("x+text"), name = "average") %>%
#'       layout(barmode = 'group')
#'   }
#'
#'   c <- manipulateWidget(
#'     plotCar(subsetCars, car),
#'     cylinders = mwSelect(c("4", "6", "8")),
#'     subsetCars = mwSharedValue(subset(mtcars, cylinders == cyl)),
#'     car = mwSelect(choices = row.names(subsetCars))
#'   )
#' }
#'
#' @export
#' @family controls
mwSharedValue <- function(expr = NULL) {
  params <- list(expr = substitute(expr))
  params$dynamic <- is.language(params$expr)
  if (!params$dynamic) value <- params$expr
  else value <- NULL
  Input(
    type = "sharedValue", value = value, label = NULL, params = params,
    display = FALSE,
    validFunc = function(x, params) {
        if(params$dynamic) params$expr
        else x
    }
  )
}

#' Group inputs in a collapsible box
#'
#' This function generates a collapsible box containing inputs. It can be useful
#' when there are a lot of inputs and one wants to group them.
#'
#' @param ... inputs that will be grouped in the box
#' @param .display expression that evaluates to TRUE or FALSE, indicating when
#'   the group should be shown/hidden.
#'
#' @return Input of type "group".
#'
#' @examples
#' if(require(dygraphs)) {
#'   mydata <- data.frame(x = 1:100, y = rnorm(100))
#'   manipulateWidget(
#'     dygraph(mydata[range[1]:range[2], ],
#'             main = title, xlab = xlab, ylab = ylab),
#'     range = mwSlider(1, 100, c(1, 100)),
#'     "Graphical parameters" = mwGroup(
#'       title = mwText("Fictive time series"),
#'       xlab = mwText("X axis label"),
#'       ylab = mwText("Y axis label")
#'     )
#'   )
#' }
#'
#' @export
#' @family controls
mwGroup <- function(..., .display = TRUE) {
  inputs <- list(...)
  if (is.null(names(inputs))) stop("All arguments need to be named.")
  for (i in inputs) if (!inherits(i, "Input")) stop("All arguments need to be Input objects.")

  Input(
    type = "group", value = list(...), params = list(),
    display = substitute(.display),
    htmlFunc = function(id, label, value, params, ns) {
      htmlElements <- lapply(value, function(x) x$getHTML(ns))

      tags$div(
        class="panel panel-default",
        tags$div(
          class="panel-heading collapsed",
          style = "cursor: pointer;",
          "data-toggle"="collapse",
          "data-target"=paste0("#panel-body-", id),
          tags$table(
            tags$tbody(
              tags$tr(
                tags$td(class = "arrow"),
                tags$td(label)
              )
            )
          )
        ),
        tags$div(
          class="panel-body collapse",
          id=paste0("panel-body-", id),
          shiny::tagList(htmlElements)
        )
      )
    }
  )
}
