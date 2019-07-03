# Purpose: Assess fire severity versus percent nighttime detections pointwise

# Purpose: plot event severity versus percent nighttime fire detections

library(tidyverse)
library(sf)
library(raster)
library(lubridate)
library(purrr)
library(here)


# Read all the data
meta <- 
  read_csv("data/data_output/event-level-severity-frap-metadata.csv") %>% 
  dplyr::select(fire_id, prop_high, max_patch_m2_high, sdc)

frap_meta <- st_read("data/data_raw/wildfire-severity_sierra-nevada-ca-usa_ypmc_1984-2017_fire-metadata.geoJSON", stringsAsFactors = FALSE) %>% 
  st_transform(3310) %>% 
  dplyr::left_join(meta, by = "fire_id")


firepoints3310 <- st_read("data/data_output/sierra-ypmc-active-fire-detections_epsg3310.gpkg")

severity_adf_filepaths <-
  list.files(path = here::here("data/data_output/wildfire-severity-active-fire-detections_sierra-nevada-ca-usa_ypmc_1984-2017"),
             full.names = TRUE)

full_dataframe <-
  severity_adf_filepaths %>%
  map(.f = read.csv,
      stringsAsFactors = FALSE) %>%
  bind_rows()

head(full_dataframe)
head(firepoints3310)

some_adf <- full_dataframe %>% filter(count_total > 0)

plot(x = some_adf$prop_night, y = some_adf$rbr, pch = 19)

# ggplot(full_dataframe %>% filter(count_total > 0), aes(x = prop_night, y = rbr)) +
  # geom_point() +
  # geom_smooth()