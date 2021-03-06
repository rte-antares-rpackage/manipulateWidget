Add more interactivity to interactive charts
================

[![CRAN Status
Badge](https://www.r-pkg.org/badges/version/manipulateWidget)](https://cran.r-project.org/web/packages/manipulateWidget/index.html)
[![CRAN Downloads
Badge](https://cranlogs.r-pkg.org/badges/manipulateWidget)](https://cran.r-project.org/web/packages/manipulateWidget/index.html)
[![Travis-CI Build
Status](https://travis-ci.org/rte-antares-rpackage/manipulateWidget.svg?branch=master)](https://travis-ci.org/github/rte-antares-rpackage/manipulateWidget)
[![Appveyor Build
Status](https://ci.appveyor.com/api/projects/status/6y3tdofl0nk7oc4g/branch/master?svg=true)](https://ci.appveyor.com/project/rte-antares-rpackage/manipulatewidget/branch/master)

`manipulateWidget` lets you create in just a few lines of R code a nice
user interface to modify the data or the graphical parameters of one or
multiple interactive charts. It is useful to quickly explore visually
some data or for package developers to generate user interfaces easy to
maintain.

![Combining widgets and some html content](vignettes/fancy-example.gif)

This R package is largely inspired by the `manipulate` package from
Rstudio. It provides the function `manipulateWidget` that can be used to
create in a very easy way a graphical interface that let the user modify
the data or the parameters of an interactive chart. Technically, the
function generates a Shiny gadget, but the user does not even have to
know what is Shiny.

## Features

  - Easily combine multiple interactive charts (`htmlwidgets`) in a
    single interactive chart with function `combineWidgets`.
  - With only a few lines of code, create a complete user interface that
    lets a user change the settings of a chart: filter the input data,
    change the model, modify the chart type or anything else.
  - Comparison mode: compare at a glance two set of parameters. For
    instance compare the same chart for two different countries or
    compare the results of several models or whatever.
  - Export to HTML or to PNG with a single click.

## Why should you use it?

All functionalities of this package can be replicated with other
packages like [shiny](https://shiny.rstudio.com/),
[flexdashboard](https://pkgs.rstudio.com/flexdashboard/),
[crosstalk](http://rstudio.github.io/crosstalk/) and others. So why
another package?

`manipulateWidget` has three advantages:

  - It is easy and fast to use. Only a few lines of `R` are necessary to
    create a user interface.
  - Code can be included in any R script. No need to create a dedicated
    .R or .Rmd file.
  - It works with all htmlwidgets. In contrast, `crosstalk` only
    supports a few of them.

`manipulateWidget` can be especially powerful for users who are
exploring some data set and want to quickly build a graphical tool to
see what is in their data. `manipulateWidget` has also some advanced
features that can be used with almost no additional code and that could
seduce some package developers: grouping inputs, conditional inputs and
comparison mode.

## Installation

The package can be installed from CRAN:

``` r
install.packages("manipulateWidget")
```

You can also install the latest development version from
github:

``` r
devtools::install_github("rte-antares-rpackage/manipulateWidget", ref="develop")
```

## Getting started

The hard part for the user is to write a code that generates an
interactive chart. Once this is done, he only has to describe what
parameter of the code should be modified by what input control. For
instance, consider the following code that identifies clusters in the
iris data set and uses package `plotly` to generate an interactive
scatter plot.

``` r
library(manipulateWidget)
library(dplyr)
library(ggplot2)
library(plotly)

data("worldEnergyUse")

plotEvoUse <- function(Country, Period = c(1960,2014)) {
  dataset <- worldEnergyUse %>%
    filter(country == Country, year >= Period[1] & year <= Period[2])

  ggplot(dataset, aes(year)) +
    geom_line(aes(y = energy_used, color = "Total energy")) +
    geom_line(aes(y = energy_fossil, color = "Fossil energy")) +
    scale_color_manual(values = c("black", "red")) +
    expand_limits(y = 0) +
    ggtitle(paste("Evolution of energy\nconsumption in", Country)) +
    xlab("") + ylab("Energy (kg of oil equivalent)") + labs(color = "") +
    theme_bw() +
    theme(plot.title = element_text(size=10)) +
    theme(axis.title.y = element_text(size=9))
}

plotEvoUse("United States") %>% ggplotly()
```

<img src="README_files/figure-gfm/plotevouse-1.png" width="600" height="400" />

We create a second function that represents the share of a given country
in the world energy consumption and population. We create also create a
custom tooltip.

``` r
tooltipText <- function(title, value) {
  sprintf("%s: %s%%", title, round(value * 100, 1))
}

plotShareUse <- function(Country, Period = c(1960, 2014)) {
  dataset <- worldEnergyUse %>%
    filter(country == Country, year %in% Period)

  ggplot(dataset) +
    facet_grid(year ~ .) +
    geom_bar(aes("Population", weight = prop_world_population,
                 text = tooltipText("Population", prop_world_population))) +
    geom_bar(aes("Energy Use", weight = prop_world_energy_used,
                 text = tooltipText("Energy Use", prop_world_energy_used))) +
    geom_bar(aes("Energy Fossil", weight = prop_world_energy_fossil,
                 text = tooltipText("Energy Fossil", prop_world_energy_fossil))) +
    ggtitle("Share of world...") +
    xlab("") + ylab("") +
    scale_y_continuous(labels = scales::percent) +
    theme_bw() +
    theme(plot.title = element_text(size=10)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

suppressWarnings(plotShareUse("Germany")) %>%
  ggplotly(tooltip = "text")
```

<img src="README_files/figure-gfm/plotshareuse-1.png" width="600" height="400" />

We can combine two charts with the helper function `combineWidgets()`.
We create a new function for clarity, but this is not a requirement.

``` r
combinedPlots <- function(Country, Period = c(1960, 2014)) {
  combineWidgets(
    plotEvoUse(Country, Period) %>% ggplotly() %>% 
      layout(
        legend = list(orientation = "h", x = 0, y = 0, yanchor = "bottom")
      ),
    plotShareUse(Country, Period) %>% ggplotly(tooltip = "text"),
    ncol = 2, colsize = c(2, 1)
  )
}

combinedPlots("Germany")
```

<img src="README_files/figure-gfm/combinewidgets-1.png" width="600" height="400" />

So we now have some R code that generates a nice interactive chart. Now
we would like to create a user interface that lets a user choose the
country and the period that he wants to visualize.

Here comes the magic of package `manipulateWidget`\! With this package,
you only have to write a few more lines of R code to achieve this
result:

``` r
manipulateWidget(
  combinedPlots(Period, Country),
  Period = mwSlider(1960, 2014, c(1960, 2014)),
  Country = mwSelect(sort(unique(worldEnergyUse$country)), "United States")
)
```

And voila\!

For more information take a look at the [package
vignette](https://cran.r-project.org/web/packages/manipulateWidget/vignettes/manipulateWidgets.html).

## License Information:

Copyright 2015-2020 RTE (France)

  - RTE: <https://www.rte-france.com>

This Source Code is subject to the terms of the GNU General Public
License, version 2 or any higher version. If a copy of the GPL-v2 was
not distributed with this file, You can obtain one at
<https://www.gnu.org/licenses/old-licenses/gpl-2.0.en.html>.
