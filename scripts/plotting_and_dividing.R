#library
library(tidyverse)
library(here)

vendor_lists <- read_rds("output/vendor_list_oj.RDS")

no_contact <- vendor_lists[[1]]
contacted <- vendor_lists[[2]]

zip <- '(?<!\\d)\\d{5}(?:[ -]\\d{4})?\\b'
  #"^[0-9]{5}?$"


View(contacted)

contacted_zips <- contacted %>%
  mutate(zip = str_extract(contacted$Address, zip))
