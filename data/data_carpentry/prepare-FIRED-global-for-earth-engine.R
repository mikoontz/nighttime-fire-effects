# Purpose: prepare North America FIRED database for Earth Engine upload

library(sf)
library(tidyverse)

fired <- sf::st_read("data/data_raw/western_hemisphere_to_may2019.gpkg")

fired_4326 <- 
  fired %>% 
  # dplyr::mutate(east_west = "west",
  #               north_south = ifelse(st_coordinates(st_centroid(geom))[, "Y"] >= 0, yes = "north", no = "south")) %>% 
  st_transform(4326) %>% 
  st_wrap_dateline() %>%
  dplyr::mutate(id = as.character(id)) %>%
  dplyr::rename(ha_burned = area_burned_ha)

dir.create("data/data_output/FIRED_global/FIRED_western-hemisphere", recursive = TRUE)

sf::st_write(x = fired_4326, 
             dsn = "data/data_output/FIRED_global/FIRED_western-hemisphere/FIRED_western-hemisphere.shp")

# fired_north <-
#   fired_4326 %>% 
#   dplyr::filter(north_south == "north")
# 
# fired_south <-
#   fired_4326 %>% 
#   dplyr::filter(north_south == "south")

dir.create("data/data_output/FIRED_global/FIRED_north-west-hemisphere", recursive = TRUE)

sf::st_write(x = fired_north, 
             dsn = "data/data_output/FIRED_global/FIRED_north-west-hemisphere/FIRED_north-west-hemisphere.shp")

dir.create("data/data_output/FIRED_global/FIRED_south-west-hemisphere", recursive = TRUE)

sf::st_write(x = fired_south, 
             dsn = "data/data_output/FIRED_global/FIRED_south-west-hemisphere/FIRED_south-west-hemisphere.shp")

