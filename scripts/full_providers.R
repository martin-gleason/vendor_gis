#full_geocode
library("tidyverse")
library("ggmap")
library(sf)
library(rgdal)
library(spdplyr)

##Load google key and providers list
key <- read_lines("keys/gmaps_api.txt")
register_google(key)

chicago_map <- get_map("Chicago, Illinois", zoom = 9, 
                       source = "google")
providers <- read_rds("output/providers.RDS")

providers <- providers %>%
  mutate(provider_id = 1:nrow(providers))


##


#load shape and geoJSon files
cpd_geojson <- file.path("~/Dropbox (Personal)/Coding Projects/javascript/simple_json/json/CPD districts.geojson")
cpd_districts <- readOGR(cpd_geojson) #spatial polygons data frame
cpd_districts <- cpd_districts  %>%
  mutate(region = map_chr(1:length(cpd_districts@polygons), function(i){
    cpd_districts@polygons[[i]]@ID
  }))

shapefile <- cpd_districts %>% broom::tidy()

cpd_districts_sf <- cpd_districts %>% st_as_sf()

##transform dataframes to sfs data frames

provider_name_coords <- providers %>%
  select(Name, provider_id, lon, lat)

providers_sf <- providers %>%
  st_as_sf(coords = c("lon", "lat"),
           crs = "+proj=longlat +datum=WGS84")

provider_by_district <- providers_sf %>%
  st_within(cpd_districts_sf, sparse = FALSE, prepared = TRUE)

provider_district_count <- cpd_districts_sf %>%
  mutate(Count = apply(provider_by_district, 2, sum))


##Select lat lon of providers

providers_coords <- providers %>%
  select(lat, lon)

#convert into sf
providers_coords_sf <- do.call("st_sfc", c(lapply(1:nrow(providers_coords),
                                                       function(i) {st_point(as.numeric(providers_coords[i, ]))}),
                                                list("crs" = 2263)))



#convert to 2163
cpd_transformed <- st_transform(cpd_districts_sf, 2263)
provider_coords_trans <- st_transform(providers_coords_sf, 2263)


#intersect 
# providers$district <- apply(st_intersects(cpd_transformed, provider_coords_trans, sparse = FALSE), 2,
#                                         function(col){
#                                           cpd_transformed[which(col), ]$dist_label
#                                         })

#blog method -- https://mattherman.info/blog/point-in-poly/

provider_in_district <- st_join(providers_sf, cpd_districts_sf, join = st_within)


chicago_map %>%
  ggmap() +
  geom_point(data = providers,
             aes(x = lon, y = lat, col = contacted))



# 
# unique_zips <- providers %>%
#   filter(contacted == TRUE) %>%
#   select((zip)) %>%
#   unique()
# 
# write_csv(unique_zips, "output/unique_zips.csv")


#final QA
providers <- provider_in_district %>%
  select(-dist_num, -region)

providers$geometry %>% str_extract(pattern = "^-?\\([.*?]")

providers <- providers %>%
  mutate(closed = FALSE)

providers <- providers %>%
  mutate(mou_sign_date = "07/10/2019")

providers[providers$Address == "7530 S South Shore Dr, Chicago, IL 60649", ]$closed <- TRUE
  
providers %>% 
  group_by(dist_label) %>% 
  summarize(n = n()) %>% 
  arrange(desc(n))

providers <- providers %>%
  inner_join(provider_name_coords, by = c("provider_id", "provider_id",
                                          "Name" = "Name")) %>%
  select(-provider_id)

write_csv(providers, "output/providers.csv")
write_rds(providers, "tidy_inputs/providers.RDS")
