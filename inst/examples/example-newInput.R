require(manipulateWidget)
require(dygraphs)
require(colourpicker)

# defined colour picker input
?colourInput
mwColourInput  <- function(value = NULL, label = NULL, ..., .display = TRUE) {
  params <- manipulateWidget:::dotsToExpr()
  value <- substitute(value)
  manipulateWidget:::Input(
    type = "color", value = value, label = label, params = params,
    display = substitute(.display),
    validFunc = function(x, params) {
      if(!is.null(params$showColour)){
        if(!params$showColour %in% c("both", "text","background")){
          params$showColour <- "both"
        }
      }
      if(!is.null(params$palette)){
        if(!params$palette %in% c("square", "limited")){
          params$palette <- "square"
        }
      }
      if (is.null(x)) return("black")
      x
    },
    htmlFunc = manipulateWidget:::htmlFuncFactory(colourpicker::colourInput),
    htmlUpdateFunc = colourpicker::updateColourInput
  )
}

mydata <- data.frame(
  year = 2000+1:100,
  series1 = rnorm(100),
  series2 = rnorm(100),
  series3 = rnorm(100)
)

manipulateWidget(
  dygraph(mydata[range[1]:range[2] - 2000, c("year", series)], main = title) %>%
    dySeries(series, drawPoints = TRUE, color = color),
  range = mwSlider(2001, 2100, c(2001, 2100)),
  series = mwSelect(c("series1", "series2", "series3")),
  color = mwColourInput(value = "red",
                        showColour = ifelse(series %in% "series1", "text", "background"),
                        palette = ifelse(series %in% "series1", "square", "limited")),
  title = mwText("Fictive time series"),
  .compare = c("title")
)
