library(leaflet)
library(tidyverse)

providers <- read_rds("tidy_inputs/providers.RDS")

providers_map <- providers %>%
  leaflet() %>%
  addTiles() %>%
  addMarkers(lat= providers$lat,
              lng = providers$lon,
              label = providers$Name)

