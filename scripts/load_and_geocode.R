#Libraries
library(here)
library(tidyverse)
library(ggmap)

vendor_list1 <- readxl::read_xlsx(file.path("/Users/marty/Dropbox (Personal)/Documents/Probation Docs/IT/C5 Project and Images/Vendor_list_OJ Mod.xlsx"),
                                 sheet = 1)

vendor_list2 <- readxl::read_xlsx(file.path("/Users/marty/Dropbox (Personal)/Documents/Probation Docs/IT/C5 Project and Images/Vendor_list_OJ Mod.xlsx"),
                                  sheet = 2)

key <- read_lines("keys/gmaps_api.txt")


register_google(key)

contact <- vendor_list1 %>%
  mutate_geocode(Address)

contact %>% glimpse()

vendor_list2$Address <- vendor_list2$Address %>%
  replace_na("NA")


no_contact_vendor <- vendor_list2 %>%
  mutate_geocode(Address)

no_contact_vendor %>% glimpse()
contact %>% glimpse()

vendor_list_oj <- list("no_contact" = no_contact_vendor,
                       "contacted" = contact)

write_rds(vendor_list_oj, "output/vendor_list_oj.RDS")
