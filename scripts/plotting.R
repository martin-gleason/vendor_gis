#library
library(tidyverse)
library(here)
library(ggmap)
library(leaflet)
library(sf)
library(rgdal)
library(spdplyr)

key <- read_lines("keys/gmaps_api.txt")

register_google(key)

### GeoJson prep
cpd_geojson <- file.path("~/Dropbox (Personal)/Coding Projects/javascript/simple_json/json/CPD districts.geojson")
cpd_districts <- readOGR(cpd_geojson) #spatial polygons data frame
cpd_districts <- cpd_districts  %>%
  mutate(region = map_chr(1:length(cpd_districts@polygons), function(i){
    cpd_districts@polygons[[i]]@ID
  }))

shapefile <- cpd_districts %>% broom::tidy()

cpd_districts_sf <- cpd_districts %>% st_as_sf()

#centroids are centers in polygons.
district_centroids <- cbind(cpd_districts_sf, 
                            st_coordinates(st_centroid(cpd_districts_sf))) %>%
    rename(lon = X,
           lat = Y)

# x <- cpd_districts_sf %>% 
#   st_centroid(.) %>%
#   st_coordinates(.) %>%
#   cbind(cpd_districts_sf) %>%
#   rename(lon = X,
#          lat = Y)
#   
  st_centroid(cpd_districts_sf)

glimpse(district_centroids)

chicago_ggmap <- get_map("Chicago, Illinois", zoom = 9)
###

vendor_lists <- read_rds("output/vendor_list_oj.RDS")

no_contact <- vendor_lists[[1]]
contacted <- vendor_lists[[2]]

contacted$Address <- contacted$Address %>%
  str_replace("#", " ")

missing <- contacted %>%
  filter(is.na(lat))

missing <- missing %>%
  mutate_geocode(Address)

missing <- missing %>%
  select(-lon, -lat) %>%
  mutate(lon = lon1, 
         lat = lat1)

contacted <- contacted %>%
  filter(!is.na(lon)) %>%
  bind_rows(missing)

contacted_sf <- contacted %>%
  st_as_sf(coords = c("lon", "lat"),
           crs = "+proj=longlat +datum=WGS84")

contacted_by_district <- contacted_sf %>%
  st_within(cpd_districts_sf, sparse = FALSE, prepared = TRUE)

df_contacted <- cpd_districts_sf %>%
  mutate(Count = apply(contacted_by_district, 2, sum))

chicago_contacted_heatmap <- ggplot(df_contacted) + 
  geom_sf(aes(fill = Count), size = 0.5,
          col = "white") + 
  geom_text(data = district_centroids,
            aes(x = lon, y = lat, label = dist_label))+
  scale_fill_viridis_c(name = "Numbers by District") +
  theme_minimal() +
  labs(title = "Service Provider by Chicago\n Police District")

ggsave("img/contacted_provider.jpg", plot = chicago_contacted_heatmap,
       width = 8.5, height = 11, units = "in")
##Location

ggmap(chicago_ggmap) +
  geom_point(aes(x = lon, y = lat),
             data = contacted)