# Purpose: prepare North America FIRED database for Earth Engine upload

library(sf)
library(tidyverse)

fired <- sf::st_read("data/data_raw/FIRED_north-america/FIRED_north-america.shp")

fired_4326 <- st_transform(fired, 4326)

dir.create("data/data_output/FIRED_north-america")
sf::st_write(fired_4326, "data/data_output/FIRED_north-america/FIRED_north-america.shp")
