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

fired <- st_read("data/data_raw/modis_event_polygons_cus.gpkg")
sort(colnames(fired))
sort(colnames(DT_sf))

name_switch <- data.frame(oldname = sort(colnames(DT_sf)),
                          newname = c("duration", "final_perimeter", "fsr_acres_per_day", "fsr_ha_per_day", "fsr_km2_per_day", "fsr_pixels_per_day", "geometry", "id", "ignition_doy", "ignition_longitude", "ignition_date", "ignition_latitude", "ignition_month", "ignition_state", "ignition_year", "l1_eco", "l1_ecoregion", "last_date", "lc", "lc_name", "max_growth_km2", "max_growth_acres", "max_growth_ha", "max_growth_date", "max_growth_pixels", "mean_growth_km2", "mean_growth_acres", "mean_growth_ha", "mean_growth_pixels", "min_growth_km2", "min_growth_acres", "min_growth_ha", "min_growth_pixels", "system:index", "total_area_acres", "total_area_ha", "total_area_km2", "total_pixels"),
                          stringsAsFactors = FALSE)
name_switch <-
  name_switch %>% 
  dplyr::filter(oldname != "geometry")

name_switch
DT_sf2 <- DT_sf

for (i in 1:nrow(name_switch)) {
  colnames(DT_sf2)[colnames(DT_sf2) == name_switch$oldname[i]] <- name_switch$newname[i]
}

st_write(obj = DT_sf2, dsn = "data/data_output/ee-FIRED_CONUS_metadata.gpkg")

