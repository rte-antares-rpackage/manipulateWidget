Add more interactivity to interactive charts
================

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/manipulateWidget)](http://cran.r-project.org/package=manipulateWidget)

This R package is largely inspired by the `manipulate` package from Rstudio. It provides the function \``manipulateWidget` that can be used to create in a very easy way a graphical interface that let the user modify the data or the parameters of an interactive chart. Technically, the function generates a Shiny gadget, but the user does not even have to know what is Shiny.

The package also provides the `combineWidgets` function to easily combine multiple interactive charts in a single view. Of course both functions can be used together.

Here is an example that uses packages `dygraphs` and `plot_ly`.

![An example of what one can do with manipulateWidgets](vignettes/fancy-example.gif)

Installation
------------

The package can be installed from Github with package `devtools`:

``` r
devtools::install_github("rte-antares-rpackage/manipulateWidget")
```

To install the latest development version:

``` r
devtools::install_github("rte-antares-rpackage/manipulateWidget", ref="develop")
```

Getting started
---------------

The main function of the package is `manipulateWidget`. It accepts an expression that generates an interactive chart (and more precisely an htmlwidget object. See <http://www.htmlwidgets.org/> if you have never heard about it) and a set of controls created with functions mwSlider, mwCheckbox... which are used to dynamically change values within the expression. Each time the user modifies the value of a control, the expression is evaluated again and the chart is updated. Consider the following code:

``` r
manipulateWidget(
  myPlotFun(country), 
  country = mwSelect(c("BE", "DE", "ES", "FR"))
)
```

It generates a graphical interface with a select input on its left with options "BE", "DE", "ES", "FR". The value of this input is mapped to the variable `country` in the expression. By default, at the beginning the value of `country` will be equal to the first choice of the input. So the function will first execute `myPlotFun("BE")` and the result will be displayed in the main panel of the interface. If the user changes the value to "FR", then the expression `myPlotFun("FR")` is evaluated and the new result is displayed.

The interface also contains a button "Done". When the user clicks on it, the last chart is returned. It can be stored in a variable, be modified by the user, saved as a html file with saveWidget from package htmlwidgets or converted to a static image file with package `webshot`.

Of course, you can create as many controls as you want. The interface of the animated example in the introduction was generated with the following code:

``` r
manipulateWidget(
  myPlotFun(distribution, range, title),
  distribution = mwSelect(choices = c("gaussian", "uniform")),
  range = mwSlider(2000, 2100, value = c(2000, 2100), label = "period"),
  title = mwText()
)
```

To see all available controls that can be added to the UI, take a look at the list of the functions of the package:

``` r
help(package = "manipulateWidget")
```

Combining widgets
-----------------

The `combineWidgets` function gives an easy way to combine interactive charts (like `par(mfrow = c(...))` or `layout` for static plots). To do it, one has simply to pass to the function the widgets to combine. In the next example, we visualize two random time series with dygraphs and combine them.

``` r
library(dygraphs)

plotRandomTS <- function(id) {
  dygraph(data.frame(x = 1:10, y = rnorm(10)), main = paste("Random plot", id))
}

combineWidgets(plotRandomTS(1), plotRandomTS(2))
```

<!--html_preserve-->

<script type="application/json" data-for="htmlwidget-fdbabd4c5060e084ff49">{"x":{"data":[{"attrs":{"title":"Random plot 1","labels":["x","y"],"legend":"auto","retainDateWindow":false,"axes":{"x":{"pixelsPerLabel":60}}},"annotations":[],"shadings":[],"events":[],"format":"numeric","data":[[1,2,3,4,5,6,7,8,9,10],[0.934922325865057,0.0318000247096504,0.626391060662156,1.32178338395484,-1.23465606386161,-0.550934990195538,-1.47372988464071,0.403657745769903,1.68311532819214,-1.76579298262496]]},{"attrs":{"title":"Random plot 2","labels":["x","y"],"legend":"auto","retainDateWindow":false,"axes":{"x":{"pixelsPerLabel":60}}},"annotations":[],"shadings":[],"events":[],"format":"numeric","data":[[1,2,3,4,5,6,7,8,9,10],[-0.885976516323238,0.891343933488062,-0.244524955219782,-0.455469664514943,1.30764743938585,-0.743977002735181,-0.564508136370994,-1.07568378803529,-2.21945430278736,-0.769759655054798]]}],"widgetType":["dygraphs","dygraphs"],"elementId":["widget515857120","widget189615976"],"html":"<div class=\"cw-container\"><div class=\"cw-subcontainer\"><div class=\"cw-content cw-by-row\"><div class=\"cw-row cw-by-row\" style=\"flex:1;-webkit-flex:1\"><div class=\"cw-col\" style=\"flex:1;-webkit-flex:1\">\n                 <div id=\"widget515857120\" class=\"cw-widget\" style=\"width:100%;height:100%\">\u003c/div>\n               \u003c/div>\u003c/div><div class=\"cw-row cw-by-row\" style=\"flex:1;-webkit-flex:1\"><div class=\"cw-col\" style=\"flex:1;-webkit-flex:1\">\n                 <div id=\"widget189615976\" class=\"cw-widget\" style=\"width:100%;height:100%\">\u003c/div>\n               \u003c/div>\u003c/div>\u003c/div>\u003c/div>\u003c/div>"},"evals":[],"jsHooks":[]}</script>
<!--/html_preserve-->
The functions tries to find the best number of columns and rows. But one can control them with parameters `nrow`and `ncol`. It is also possible to control their relative size with parameters `rowsize` and `colsize`. To achieve complex layouts, it is possible to use nested combined widgets. Here is an example of a complex layout. (Note that this is only a screenshot of the result. In reality, these charts are interactive).

``` r
combineWidgets(
  ncol = 2, colsize = c(2, 1),
  plotRandomTS(1),
  combineWidgets(
    ncol = 1,
    plotRandomTS(2),
    plotRandomTS(3),
    plotRandomTS(4)
  )
)
```

<!--html_preserve-->

<script type="application/json" data-for="htmlwidget-b65ed905d28263ba4007">{"x":{"data":[{"attrs":{"title":"Random plot 1","labels":["x","y"],"legend":"auto","retainDateWindow":false,"axes":{"x":{"pixelsPerLabel":60}}},"annotations":[],"shadings":[],"events":[],"format":"numeric","data":[[1,2,3,4,5,6,7,8,9,10],[2.30606488325825,0.64602788897341,-0.484473935235565,1.15612425767463,3.28415514853191,0.560277790217721,1.02796422580781,0.91781837240338,-0.278799118918421,-0.672089136128041]]},{"data":[{"attrs":{"title":"Random plot 2","labels":["x","y"],"legend":"auto","retainDateWindow":false,"axes":{"x":{"pixelsPerLabel":60}}},"annotations":[],"shadings":[],"events":[],"format":"numeric","data":[[1,2,3,4,5,6,7,8,9,10],[2.28044651012501,-0.302173996743686,-0.417289599617577,0.600716234995226,-1.95970257110772,0.477605364667148,0.888791363475738,-0.908000238133934,0.161082396682496,0.111606227350367]]},{"attrs":{"title":"Random plot 3","labels":["x","y"],"legend":"auto","retainDateWindow":false,"axes":{"x":{"pixelsPerLabel":60}}},"annotations":[],"shadings":[],"events":[],"format":"numeric","data":[[1,2,3,4,5,6,7,8,9,10],[-1.23860081231025,-1.52848935621504,0.384415174289273,0.0192202954003406,-0.884787992146554,0.0460779389062596,0.110373847529488,1.31569303534349,-0.293832337102516,-0.519339821964301]]},{"attrs":{"title":"Random plot 4","labels":["x","y"],"legend":"auto","retainDateWindow":false,"axes":{"x":{"pixelsPerLabel":60}}},"annotations":[],"shadings":[],"events":[],"format":"numeric","data":[[1,2,3,4,5,6,7,8,9,10],[-0.0408064771692986,1.87549509354529,-0.804659623275489,-0.953462334273312,-1.30215009999655,-1.15400044986933,-0.364831724800917,-0.174484536451119,-0.814612392341363,0.029293217610813]]}],"widgetType":["dygraphs","dygraphs","dygraphs"],"elementId":["widget476224926","widget135104778","widget499718663"],"html":"<div class=\"cw-container\"><div class=\"cw-subcontainer\"><div class=\"cw-content cw-by-row\"><div class=\"cw-row cw-by-row\" style=\"flex:1;-webkit-flex:1\"><div class=\"cw-col\" style=\"flex:1;-webkit-flex:1\">\n                 <div id=\"widget476224926\" class=\"cw-widget\" style=\"width:100%;height:100%\">\u003c/div>\n               \u003c/div>\u003c/div><div class=\"cw-row cw-by-row\" style=\"flex:1;-webkit-flex:1\"><div class=\"cw-col\" style=\"flex:1;-webkit-flex:1\">\n                 <div id=\"widget135104778\" class=\"cw-widget\" style=\"width:100%;height:100%\">\u003c/div>\n               \u003c/div>\u003c/div><div class=\"cw-row cw-by-row\" style=\"flex:1;-webkit-flex:1\"><div class=\"cw-col\" style=\"flex:1;-webkit-flex:1\">\n                 <div id=\"widget499718663\" class=\"cw-widget\" style=\"width:100%;height:100%\">\u003c/div>\n               \u003c/div>\u003c/div>\u003c/div>\u003c/div>\u003c/div>"}],"widgetType":["dygraphs","combineWidgets"],"elementId":["widget802586077","widget615270396"],"html":"<div class=\"cw-container\"><div class=\"cw-subcontainer\"><div class=\"cw-content cw-by-row\"><div class=\"cw-row cw-by-row\" style=\"flex:1;-webkit-flex:1\"><div class=\"cw-col\" style=\"flex:2;-webkit-flex:2\">\n                 <div id=\"widget802586077\" class=\"cw-widget\" style=\"width:100%;height:100%\">\u003c/div>\n               \u003c/div><div class=\"cw-col\" style=\"flex:1;-webkit-flex:1\">\n                 <div id=\"widget615270396\" class=\"cw-widget\" style=\"width:100%;height:100%\">\u003c/div>\n               \u003c/div>\u003c/div>\u003c/div>\u003c/div>\u003c/div>"},"evals":[],"jsHooks":[]}</script>
<!--/html_preserve-->
Even if the main use of `combineWidgets` is to combine `htmlwidgets`, it can also display text or html tags. It can be useful to include comments in a chart. Moreover it has arguments to add a title and to add some html content in the sides of the chart.

``` r
combineWidgets(
  plotRandomTS(1),
  plotRandomTS(2),
  plotRandomTS(3),
  plotRandomTS(4),
  title = "Four random plots",
  header = "Here goes the header content. <span style='color:red'>It can include html code</span>.",
  footer = "Here goes the footer content.",
  leftCol = "<div style='margin-top:150px;'>left column</div>",
  rightCol = "<div style='margin-top:150px;'>right column</div>"
)
```

<!--html_preserve-->

<script type="application/json" data-for="htmlwidget-fdfd617256b7b91c0658">{"x":{"data":[{"attrs":{"title":"Random plot 1","labels":["x","y"],"legend":"auto","retainDateWindow":false,"axes":{"x":{"pixelsPerLabel":60}}},"annotations":[],"shadings":[],"events":[],"format":"numeric","data":[[1,2,3,4,5,6,7,8,9,10],[-1.51611744312392,-0.281450246741991,-0.314440419025183,1.40071118798832,0.44851572301546,2.02562755079983,1.43144141431702,1.29607865866132,0.0346152474405239,0.652399161187572]]},{"attrs":{"title":"Random plot 2","labels":["x","y"],"legend":"auto","retainDateWindow":false,"axes":{"x":{"pixelsPerLabel":60}}},"annotations":[],"shadings":[],"events":[],"format":"numeric","data":[[1,2,3,4,5,6,7,8,9,10],[-0.79099967520803,1.98034936228038,2.09447550565824,-1.92582670658141,-1.13738097215989,-0.0154850345619225,-0.95806158708724,1.23586850020771,0.284304060163905,-1.4987432250075]]},{"attrs":{"title":"Random plot 3","labels":["x","y"],"legend":"auto","retainDateWindow":false,"axes":{"x":{"pixelsPerLabel":60}}},"annotations":[],"shadings":[],"events":[],"format":"numeric","data":[[1,2,3,4,5,6,7,8,9,10],[0.103967496289492,0.229007315481627,-0.117469901174365,1.58023739216621,-0.186820413003205,0.171813916397221,-0.403293319708517,-0.148620281904321,0.0643056211812295,-1.30636674762002]]},{"attrs":{"title":"Random plot 4","labels":["x","y"],"legend":"auto","retainDateWindow":false,"axes":{"x":{"pixelsPerLabel":60}}},"annotations":[],"shadings":[],"events":[],"format":"numeric","data":[[1,2,3,4,5,6,7,8,9,10],[-0.100596509024331,0.41750392775675,-0.447644221839839,-1.47393310640497,-1.50350155598511,1.92135444711921,1.25660804478265,-0.306433137678474,-0.562085834773227,-0.463927515680843]]}],"widgetType":["dygraphs","dygraphs","dygraphs","dygraphs"],"elementId":["widget885053086","widget453799014","widget310785766","widget789728454"],"html":"<div class=\"cw-container\"><div><h2 class=\"cw-title\" style=\"\">Four random plots\u003c/h2>\u003c/div><div>Here goes the header content. <span style='color:red'>It can include html code\u003c/span>.\u003c/div><div class=\"cw-subcontainer\"><div style='height:100%'><div style='margin-top:150px;'>left column\u003c/div>\u003c/div><div class=\"cw-content cw-by-row\"><div class=\"cw-row cw-by-row\" style=\"flex:1;-webkit-flex:1\"><div class=\"cw-col\" style=\"flex:1;-webkit-flex:1\">\n                 <div id=\"widget885053086\" class=\"cw-widget\" style=\"width:100%;height:100%\">\u003c/div>\n               \u003c/div><div class=\"cw-col\" style=\"flex:1;-webkit-flex:1\">\n                 <div id=\"widget453799014\" class=\"cw-widget\" style=\"width:100%;height:100%\">\u003c/div>\n               \u003c/div>\u003c/div><div class=\"cw-row cw-by-row\" style=\"flex:1;-webkit-flex:1\"><div class=\"cw-col\" style=\"flex:1;-webkit-flex:1\">\n                 <div id=\"widget310785766\" class=\"cw-widget\" style=\"width:100%;height:100%\">\u003c/div>\n               \u003c/div><div class=\"cw-col\" style=\"flex:1;-webkit-flex:1\">\n                 <div id=\"widget789728454\" class=\"cw-widget\" style=\"width:100%;height:100%\">\u003c/div>\n               \u003c/div>\u003c/div>\u003c/div><div style='height:100%'><div style='margin-top:150px;'>right column\u003c/div>\u003c/div>\u003c/div><div>Here goes the footer content.\u003c/div>\u003c/div>"},"evals":[],"jsHooks":[]}</script>
<!--/html_preserve-->
Advanced usage
--------------

### Grouping controls

If you have a large number of inputs, you can easily group them. To do so, simply pass to the function `manipulateWidget` a list of inputs instead of passing directly the inputs. Here is a toy example. Groups are by default collapsed and user can click on their title to display/collapse then.

``` r
mydata <- data.frame(x = 1:100, y = rnorm(100))
manipulateWidget(
  dygraph(mydata[range[1]:range[2], ],
          main = title, xlab = xlab, ylab = ylab),
  range = mwSlider(1, 100, c(1, 100)),
  "Graphical parameters" = list(
    title = mwText("Fictive time series"),
    xlab = mwText("X axis label"),
    ylab = mwText("Y axis label")
  )
)
```

![Grouping inputs](vignettes/groups-inputs.gif)

### Conditional inputs

Sometimes some inputs are relevant only if other inputs have some value. `manipulateWidget`provides a way to show/hide inputs conditionally to the value of the other inputs thanks to parameter `.display`. This parameter expects a named list of expressions. The names are the ones of the inputs to show/hide and the expressions can include any input and have to evaluate to `TRUE/FALSE`. Here is a toy example, using package `plot_ly`. User can choose points or lines to represent some data. If he chooses lines, then an input appears to let him choose the width of the lines.

``` r
mydata <- data.frame(x = 1:100, y = rnorm(100))

myPlot <- function(type, lwd) {
  if (type == "points") {
    plot_ly(mydata, x= ~x, y = ~y, type = "scatter", mode = "markers")
  } else {
    plot_ly(mydata, x= ~x, y = ~y, type = "scatter", mode = "lines", 
            line = list(width = lwd))
  }
}

manipulateWidget(
  myPlot(type, lwd),
  type = mwSelect(c("points", "lines"), "points"),
  lwd = mwSlider(1, 10, 1),
  .display = list(lwd = type == "lines")
)
```

![Conditional inputs](vignettes/conditional-inputs.gif)

### Updating a widget

The "normal" use of `manipulateWidget` is to provide an expression that always return an `htmlwidget`. In such case, every time the user changes the value of an input, the current widget is destroyed and a new one is created and rendered. This behavior is not optimal and sometimes it can be painful for the user: consider for instance an interactive map. Each time user changes an input, the map is destroyed and created again, then zoom and location on the map are lost every time.

Some packages provide functions to update a widget that has already been rendered. This is the case for instance for package `leaflet` with the function `leafletProxy`. To use such functions, `manipulateWidget` evaluates the parameter `.expr` with two extra variables:

-   `.initial`: `TRUE` if the expression is evaluated for the first time and then the widget has not been rendered yet, `FALSE` if the widget has already been rendered.

-   `.session`: A shiny session object.

Moreover the ID of the rendered widget is always "output". Then it is quite easy to write an expression that initializes a widget when it is evaluated the first time and then that updates this widget. Here is an example using package `leaflet`.

``` r
lon <- rnorm(10, sd = 20)
lat <- rnorm(10, sd = 20)

myMapFun <- function(radius, color, initial, session) {
  if (initial) {
    # Widget has not been rendered
    map <- leaflet() %>% addTiles()
  } else {
    # widget has already been rendered
    map <- leafletProxy("output", session) %>% clearMarkers()
  }

  map %>% addCircleMarkers(lon, lat, radius = radius, color = color)
}

manipulateWidget(myMapFun(radius, color, .initial, .session),
                 radius = mwSlider(5, 30, 10),
                 color = mwSelect(c("red", "blue", "green")))
```

![Conditional inputs](vignettes/update-widget.gif)

By the way
----------

Here is the complete code to generate the animated example in the introduction:

``` r
library(dygraphs)
library(plotly)
library(manipulateWidget)

myPlotFun <- function(distribution, range, title) {
  randomFun <- switch(distribution, gaussian = rnorm, uniform = runif)
  myData <- data.frame(
    year = seq(range[1], range[2]),
    value = randomFun(n = diff(range) + 1)
  )
  combineWidgets(
    ncol = 2, colsize = c(2, 1),
    dygraph(myData, main = title),
    combineWidgets(
      plot_ly(x = myData$value, type = "histogram"),
      paste(
        "The graph on the left represents a random time series generated using a <b>",
        distribution, "</b>distribution function.<br/>",
        "The chart above represents the empirical distribution of the generated values."
      )
    )
  )
  
}

manipulateWidget(
  myPlotFun(distribution, range, title),
  distribution = mwSelect(choices = c("gaussian", "uniform")),
  range = mwSlider(2000, 2100, value = c(2000, 2100), label = "period"),
  title = mwText()
)
```
