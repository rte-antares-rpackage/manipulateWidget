## code to prepare `DATASET` dataset goes here
library(wbstats)
library(dplyr)

new_cache <- wbcache("en")
wbsearch("energy", cache = new_cache) %>% View()

indicators <- c(
  population = "SP.POP.TOTL",
  energy_used_per_capita = "EG.USE.PCAP.KG.OE",
  energy_imported_prop = "EG.IMP.CONS.ZS",
  energy_fossil_prop = "EG.USE.COMM.FO.ZS"
)

data_source <- wb("countries_only", indicators, return_wide = TRUE)

worldEnergyUse <- data_source %>%
  select_at(c("country", "iso2c", year = "date", indicators)) %>%
  mutate(
    year = as.numeric(year),
    energy_fossil_prop = energy_fossil_prop / 100,
    energy_imported_prop = energy_imported_prop / 100
  ) %>%
  filter(year <= 2014) %>%
  mutate(energy_used = energy_used_per_capita * population) %>%
  mutate(energy_fossil = energy_used * energy_fossil_prop) %>%
  group_by(year) %>%
  mutate(
    prop_world_energy_used = energy_used / sum(energy_used, na.rm = TRUE),
    prop_world_energy_fossil = energy_fossil / sum(energy_fossil, na.rm = TRUE),
    prop_world_population = energy_used / sum(population, na.rm = TRUE)
  ) %>%
  ungroup()

# Add country region, longitude and lattitude
countries <- new_cache$countries %>%
  select(iso2c, long, lat, region)

worldEnergyUse <- worldEnergyUse %>%
  inner_join(countries, by = "iso2c")

usethis::use_data(worldEnergyUse, overwrite = TRUE)
