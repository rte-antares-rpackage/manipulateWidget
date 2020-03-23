# Basic example with fake data
if (require(dygraphs)) {
  mydata <- data.frame(period = 1:100, value = rnorm(100))
  manipulateWidget(dygraph(mydata[range[1]:range[2], ], main = title),
                   range = mwSlider(1, 100, c(1, 100)),
                   title = mwText("Fictive time series"))
}

# Let use manipulateWidget to explore the evolution of energy consumption in
# the world
data("worldEnergyUse")

if (require(plotly)) {
  # Function that generates a chart representing the evolution of energy
  # consumption per country. Creating a function is not necessary. We do it
  # for clarity and reuse in the different examples.
  plotEnergyUse <- function(Country, Period, lwd = 2, col = "gray") {
    dataset <- subset(
      worldEnergyUse,
      country == Country & year >= Period[1] & year <= Period[2]
    )
    plot_ly(dataset) %>%
      add_lines(~year, ~energy_used, line = list(width = lwd, color = col)) %>%
      layout(title = paste("Energy used in", Country))
  }

  # Launch the interactive visualisation
  manipulateWidget(
    plotEnergyUse(Country, Period),
    Period = mwSlider(1960, 2014, c(1960, 2014)),
    Country = mwSelect(sort(unique(worldEnergyUse$country)), "United States")
  )

  # Directly start comparison mode
  manipulateWidget(
    plotEnergyUse(Country, Period),
    Period = mwSlider(1960, 2014, c(1960, 2014)),
    Country = mwSelect(sort(unique(worldEnergyUse$country))),
    .compare = list(Country = c("United States", "China")),
    .compareOpts = compareOptions(ncol = 2)
  )

  # Dynamic input parameters
  #-------------------------
  # The arguments of an input can depend on the values of other inputs.
  # In this example, when the user changes the region, the choices of input
  # "Country" are updated with the countries of that region.

  # First we create a list that contains for each region the countries in that
  # retion
  refRegions <- by(worldEnergyUse$country, worldEnergyUse$region,
                   function(x) as.character(sort(unique(x))))

  manipulateWidget(
    plotEnergyUse(Country, Period),
    Period = mwSlider(1960, 2014, c(1960, 2014)),
    Region = mwSelect(sort(unique(worldEnergyUse$region))),
    Country = mwSelect(choices = refRegions[[Region]])
  )

  # Grouping inputs
  #----------------
  # Inputs can be visually grouped with function mwGroup()
  manipulateWidget(
    plotEnergyUse(Country, Period, lwd, col),
    Period = mwSlider(1960, 2014, c(1960, 2014)),
    Country = mwSelect(sort(unique(worldEnergyUse$country)), "United States"),
    `Graphical Parameters` = mwGroup(
      lwd = mwSlider(1,10, 2, label = "Line Width"),
      col = mwSelect(choices = c("gray", "black", "red"))
    )
  )

  # Conditional inputs
  #-------------------
  # Inputs can be displayed or hidden depending on the state of other inputs.
  # In this example, user can choose to display the level of aggregation
  # (region or country). Depending on the choixe, the application displays
  # input Region or input Country.
  plotEnergyUseRegion <- function(Region, Period, lwd = 2, col = "gray") {
    dataset <- subset(
      worldEnergyUse,
      region == Region & year >= Period[1] & year <= Period[2]
    )
    dataset <- aggregate(energy_used ~ year, sum, data = dataset)

    plot_ly(dataset) %>%
      add_lines(~year, ~energy_used, line = list(width = lwd, color = col)) %>%
      layout(title = paste("Energy used in", Region))
  }

  manipulateWidget(
    {
      if (Level == "Region") {
        plotEnergyUseRegion(Region, Period)
      } else {
        plotEnergyUse(Country, Period)
      }
    },
    Period = mwSlider(1960, 2014, c(1960, 2014)),
    Level = mwSelect(c("Region", "Country")),
    Region = mwSelect(sort(unique(worldEnergyUse$region)),
                      .display = Level == "Region"),
    Country = mwSelect(sort(unique(worldEnergyUse$country)),
                       .display = Level == "Country")
  )

}

# Advanced Usage
# --------------
# When .expr is evaluated with tehnical variables:
# .initial: is it the first evaluation?
# .outputId: integer representing the id of the chart
# .output: shiny output id
# .session: shiny session
# They can be used to update an already rendered widget instead of replacing
# it each time an input value is modified.
#
# In this example, we represent on a map, the energy use of countries.
# When the user changes an input, the map is not redrawn. Only the circle
# markers are updated.
if (require(leaflet)) {
  plotMap <- function(Year, MaxRadius = 30, .initial, .session, .output) {
    dataset <- subset(worldEnergyUse, year == Year)
    radius <- sqrt(dataset$energy_used) /
      max(sqrt(worldEnergyUse$energy_used), na.rm = TRUE) * MaxRadius

    if (.initial) { # map has not been rendered yet
      map <- leaflet() %>% addTiles()
    } else { # map already rendered
      map <- leafletProxy(.output, .session) %>% clearMarkers()
    }

    map %>% addCircleMarkers(dataset$long, dataset$lat, radius = radius,
                             color = "gray", weight = 0, fillOpacity = 0.7)
  }

  manipulateWidget(
    plotMap(Year, MaxRadius, .initial, .session, .output),
    Year = mwSlider(1960, 2014, 2014),
    MaxRadius = mwSlider(10, 50, 20)
  )
}
