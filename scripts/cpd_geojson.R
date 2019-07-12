#geocoding and police district 
library(tidyverse)
library(ggmap)
library(rgdal)
library(sf)
library(spdplyr)


cpd_geojson <- file.path("~/Dropbox (Personal)/Coding Projects/javascript/simple_json/json/CPD districts.geojson")
cpd_districts <- readOGR(cpd_geojson) #spatial polygons data frame
cpd_districts <- cpd_districts  %>%
  mutate(region = map_chr(1:length(cpd_districts@polygons), function(i){
    cpd_districts@polygons[[i]]@ID
  }))

shapefile <- cpd_districts %>% broom::tidy()

cpd_districts_sf <- cpd_districts %>% st_as_sf()

contacted_zips_coords_sf <- do.call("st_sfc", c(lapply(1:nrow(contacted_zips_coords),
                                                       function(i) {st_point(as.numeric(contacted_zips_coords[i, ]))}),
                                                list("crs" = 4326)))

zips_transformed <- st_transform(contacted_zips_coords_sf, 2163)
df_contacted_sf <- df_contacted %>% st_transform(2163)

contacted_zips_coords$district <- apply(st_intersects(df_contacted_sf, zips_transformed, sparse = FALSE), 2,
                                        function(col){
                                          df_contacted_sf[which(col), ]$dist_num[1]
                                        })