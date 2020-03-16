library(sf)
library(spData)
library(spDataLarge)
library(tidyverse)

# All countries in Eastern Europe with a population (pop) greater than 5 million
e_europe_5mil = world %>%
  filter(subregion == "Eastern Europe" & pop > 5e6) %>%
  dplyr::select(pop) %>%
  plot(main = "All countries in Eastern Europe with a population (pop) greater than 5 million")

# All countries in Africa with a GDP per capita (gdpPercap) of (more than) $2000
world %>%
  filter(continent == "Africa" & gdpPercap > 2000) %>%
  dplyr::select(gdpPercap) %>%
  plot(main = "All countries in Africa with a GDP per capita (gdpPercap) of (more than) $2000")

# All countries in Central America with a life expectancy less (lifeExp) than 75 years old.
world %>%
  filter(subregion == "Central America" & lifeExp < 75) %>%
  dplyr::select(lifeExp) %>%
  plot(main = "All countries in Central America with a life expectancy less (lifeExp) than 75 years old.")

# All countries with a political condition (type) of dispute
world %>%
  filter(type == "Disputed") %>%
  dplyr::select(type) %>%
  plot(main = "All countries with a political condition (type) of dispute")

