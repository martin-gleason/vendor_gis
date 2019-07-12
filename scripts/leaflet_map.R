library(leaflet)
library(tidyverse)
library(here)

providers <- read_rds(here("tidy_inputs/providers.RDS"))

providers_map <- providers %>%
  leaflet() %>%
  addTiles() %>%
  addMarkers(lat= providers$lat,
              lng = providers$lon,
              label = providers$Name)

