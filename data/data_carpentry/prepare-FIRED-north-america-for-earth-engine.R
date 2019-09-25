# Purpose: prepare North America FIRED database for Earth Engine upload

library(sf)
library(tidyverse)

fired <- sf::st_read("data/data_raw/modis_events/modis_events.shp")

fired_4326 <- 
  fired %>% 
  st_transform(crs = 4326) %>% 
  st_wrap_dateline()
  
dir.create("data/data_output/FIRED_north-america")
sf::st_write(fired_4326, "data/data_output/FIRED_north-america/FIRED_north-america.shp")
