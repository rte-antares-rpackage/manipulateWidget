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

data_source <- wb("countries_only", indicators, return_wide = TRUE, )

worldEnergyUse <- data_source %>%
  select_at(c("country", "iso2c", year = "date", indicators)) %>%
  mutate(
    country = as.character(country),
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
    prop_world_population = population / sum(population, na.rm = TRUE)
  ) %>%
  ungroup()

# Add country region, longitude and lattitude
countries <- new_cache$countries %>%
  select(iso2c, long, lat, region) %>%
  mutate(
    region = as.character(region),
    long = as.numeric(long),
    lat = as.numeric(lat)
  ) %>%
  filter(!is.na(long))

worldEnergyUse <- worldEnergyUse %>%
  inner_join(countries, by = "iso2c")

# Remove countries that have only NA values
countries_to_keep <- worldEnergyUse %>%
  group_by(iso2c) %>%
  summarize(not_na = sum(!is.na(energy_used))) %>%
  filter(not_na > 0) %>%
  pull(iso2c)

worldEnergyUse <- worldEnergyUse %>%
  filter(iso2c %in% countries_to_keep)

worldEnergyUse <- as.data.frame(worldEnergyUse)

usethis::use_data(worldEnergyUse, overwrite = TRUE)
