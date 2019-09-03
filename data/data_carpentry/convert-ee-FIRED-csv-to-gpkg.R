# Purpose: Make .csv output of Google Earth Engine that includes EE-assigned system:index values per FIRED event spatial

library(sf)
library(tidyverse)
library(data.table)

meta <-
  read_csv("data/data_output/ee-FIRED_CONUS_metadata.csv")

DT <- as.data.table(meta)

(start_time <- Sys.time())
DT[, geometry := st_as_sfc(.geo, GeoJSON = TRUE)]
DT[, .geo := NULL]
DT_sf <- st_as_sf(DT)
st_write(obj = DT_sf, dsn = "data/data_output/ee-FIRED_CONUS_metadata.gpkg")

(Sys.time() - start_time)

