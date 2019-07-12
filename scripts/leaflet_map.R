library(leaflet)
library(tidyverse)
library(here)

providers <- read_rds(here::here("tidy_inputs/providers.RDS"))

providers_map <- providers %>%
  leaflet() %>%
  addTiles() %>%
  addMarkers(lat= providers$lat,
              lng = providers$lon,
              label = providers$Name)

##
city_numbers <- providers %>% 
  filter(!is.na(dist_label)) %>%
  group_by(dist_label) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))
