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

manipulateWidget(
  combineWidgets(
    plotEvoUse(Country, Period) %>% ggplotly() %>%
      layout(
        legend = list(orientation = "h", x = 0, y = 0, yanchor = "bottom"),
        margin = list(t = 40, b = 0)
      ),
    suppressWarnings(plotShareUse(Country, Period)) %>%
      ggplotly(tooltip = "text") %>%
      layout(margin = list(t = 40, b=0)),
    ncol = 2, colsize = c(2, 1)
  ),
  Period = mwSlider(1960, 2014, c(1960, 2014)),
  Country = mwSelect(sort(unique(worldEnergyUse$country)), "United States")
)
