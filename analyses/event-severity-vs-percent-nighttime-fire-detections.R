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

# severity_adf_filepaths <-
#   list.files(path = here::here("data/data_output/wildfire-severity-active-fire-detections_sierra-nevada-ca-usa_ypmc_1984-2017"),
#              full.names = TRUE)
# 
# full_dataframe <- 
#   severity_adf_filepaths %>% 
#   map(.f = read.csv,
#       stringsAsFactors = FALSE) %>% 
#   bind_rows()
# 
# head(full_dataframe)
# head(firepoints3310)

frap_meta_post_2000 <-
  frap_meta %>% 
  tidyr::unite(col = "alarm_date", alarm_year, alarm_month, alarm_day, remove = FALSE) %>% 
  dplyr::mutate(alarm_date = ymd(alarm_date)) %>% 
  dplyr::filter(alarm_date >= ymd("2000-11-01")) 

fire_ids_post_2000 <-
  frap_meta_post_2000 %>% 
  dplyr::filter(!is.na(fire_id)) %>% 
  pull(fire_id) %>% 
  as.character()

# get the filenames of the different rasters and match to their attributes
fire_raster_meta3310 <- 
  list.files("data/data_raw/wildfire-severity_sierra-nevada-ca-usa_ypmc_1984-2017_rasters/") %>% 
  str_split(pattern = "_", simplify = TRUE) %>% 
  as.data.frame(stringsAsFactors = FALSE) %>% 
  as_tibble() %>% 
  setNames(c("date", "iter", "fire_id", "epsg.tif")) %>% 
  dplyr::filter(fire_id %in% fire_ids_post_2000) %>% 
  tidyr::unite(col = "filename", date, iter, fire_id, epsg.tif, sep = "_", remove = FALSE) %>% 
  dplyr::select(-iter, -epsg.tif) %>% 
  dplyr::left_join(frap_meta_post_2000, by = "fire_id") %>% 
  st_as_sf()

fire_raster_meta3310$active_fire_count <- 0
fire_raster_meta3310$active_fire_count_day <- 0
fire_raster_meta3310$active_fire_count_night <- 0

MODIS_temporal_cluster_constant <- 11
spatial_hits3310 <- vector(length = nrow(fire_raster_meta3310), mode = "list")

for (i in seq_along(fire_raster_meta3310$fire_id)) {
  
  this_fire_meta3310 <- 
    fire_raster_meta3310[i, ] %>% 
    unite(col = "alarm_date", alarm_year, alarm_month, alarm_day, sep = "-", remove = FALSE) %>% 
    dplyr::mutate(alarm_date = ymd(alarm_date))
  
  spatial_hits3310[[i]] <-
    firepoints3310[this_fire_meta3310, ] %>% 
    dplyr::filter(year == this_fire_meta3310$alarm_year) %>% 
    dplyr::filter(ymd(ACQ_DATE) >= this_fire_meta3310$alarm_date - MODIS_temporal_cluster_constant,
                  ymd(ACQ_DATE) <= this_fire_meta3310$alarm_date + period(num = 1, units = "year") + MODIS_temporal_cluster_constant)
  
  fire_raster_meta3310$active_fire_count[i] <- nrow(spatial_hits3310[[i]])
  fire_raster_meta3310$active_fire_count_day[i] <- nrow(spatial_hits3310[[i]] %>% dplyr::filter(DAYNIGHT == "D"))
  fire_raster_meta3310$active_fire_count_night[i] <- nrow(spatial_hits3310[[i]] %>% dplyr::filter(DAYNIGHT == "N"))
}

afd_frap_meta <-
  fire_raster_meta3310 %>% 
  dplyr::filter(active_fire_count > 0) %>% 
  mutate(prop_night = active_fire_count_night / active_fire_count)

afd_frap_meta

prop_high_pct_night_gg <-
  ggplot(afd_frap_meta, aes(x = prop_night, y = prop_high)) +
  geom_point() +
  geom_smooth() +
  labs(x = "Proportion of nighttime active fire detections",
       y = "Proportion of event at high severity (>90% mortality)")

max_patch_high_pct_night_gg <-
  ggplot(afd_frap_meta, aes(x = prop_night, y = max_patch_m2_high)) +
  geom_point() +
  geom_smooth() +
  scale_y_log10() +
  labs(x = "Proportion of nighttime active fire detections",
       y = "Maximum high severity patch size (m^2)")

ggsave(filename = "figures/prop_high_pct_night.png", plot = prop_high_pct_night_gg)
ggsave(filename = "figures/max_patch_high_pct_night.png", plot = max_patch_high_pct_night_gg)
