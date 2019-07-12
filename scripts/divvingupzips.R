#library
library(tidyverse)
library(here)
library(ggmap)

vendor_lists <- read_rds("output/vendor_list_oj.RDS")


no_contact <- vendor_lists[[1]]
contacted <- vendor_lists[[2]]

# zip <- '(?<!\\d)\\d{5}(?:[ -]\\d{4})?\\b'
# zip2 <- "?<=\\d{5}"

key <- read_lines("keys/gmaps_api.txt")
register_google(key)

# Recleaning
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

##

contacted_zips <- contacted %>%
  mutate(zip = str_extract(contacted$Address, "[6]\\d\\d\\d\\d"),
         zip = as.factor(zip),
         contacted = TRUE) %>%
  select(Name, Address, phone_number = `Phone #`, Contact, email = `Contact Email`,
         lon, lat, zip, contacted)

contacted_zips_coords <- contacted_zips %>%
  select("lon", "lat")

contacted_zips_coords$district <- apply(contacted_zips_coords, 1, function(row){
  step1 <- st_transform(df_contacted, 2163)
  coords <- as.data.frame(matrix(row, nrow = 1,
                                 dimnames = list("", c("x", "y"))))
  dis_sf <- st_transform(st_sfc(st_point(row), crs = 4326), 2163)
  step1[which(st_intersects(dis_sf, step1, sparse=FALSE)), ]$dist_num
})

contacted_zips_coords_sf <- do.call("st_sfc", c(lapply(1:nrow(contacted_zips_coords),
                                                       function(i) {st_point(as.numeric(contacted_zips_coords[i, ]))}),
                                                list("crs" = 4326)))

zips_transformed <- st_transform(contacted_zips_coords_sf, 2163)
df_contacted_sf <- df_contacted %>% st_transform(2163)

contacted_zips_coords$district <- apply(st_intersects(df_contacted_sf, zips_transformed, sparse = FALSE), 2,
                                       function(col){
                                         df_contacted_sf[which(col), ]$dist_num[1]
                                       })

contacted_with_zips <- contacted_zips %>%
  left_join(contacted_zips_coords, by = c("lon" = "lon", "lat" = "lat")) %>%
  select(Name, Address, phone_number, Contact, email, lon, lat, zip, contacted, district)


jeannie <- contacted_with_zips %>%
  filter(district %in% jb)

# contacted_zips <- contacted_zips %>%
#   select(1, 2)

View(no_contact)

no_contact <- no_contact %>%
  select(Name, Address, phone_number = `Phone #`, 
         Contact, email = `Contact Email`,lon, lat) %>%
  mutate(contacted = FALSE)

na_no_contact <- no_contact %>%
  filter(is.na(lon)) 

na_no_contact$Address <- str_replace(na_no_contact$Address, "#", " ")

na_no_contact <- na_no_contact %>%
  select(c(-lat, -lon)) %>%
  mutate_geocode(Address)

no_contact <- no_contact %>%
  filter(!is.na(lon)) %>%
  bind_rows(na_no_contact) %>%
  mutate(zip = str_extract(Address, "[6]\\d\\d\\d\\d"),
         zip = as.factor(zip)) %>%
  select(Name, Address, phone_number, Contact, email,
         lon, lat, zip, contacted)
  

providers <- contacted_zips %>%
  bind_rows(no_contact)

#identifying po preferences
jb <- c("15", "25", "14", "10", "11")

df_contacted %>%
  filter(dist_num %in% jb)

write_rds(providers, "output/providers.RDS")

