# Purpose: Get a few measures of event-level severity

library(tidyverse)
library(sf)
library(lubridate)

md <- st_read("data/data_raw/wildfire-severity_sierra-nevada-ca-usa_ypmc_1984-2017_fire-metadata.geoJSON", stringsAsFactors = FALSE) %>% st_transform(3310)

sdc <- 
  read_csv("data/data_output/high-severity-fires-with-sdc.csv") %>% 
  dplyr::select(fire_id, sdc)

fires_by_fire <- read_csv("data/data_output/polygonized-fires-by-fire.csv")
fires_by_severity <- read.csv("data/data_output/polygonized-fires-by-severity-classes.csv", stringsAsFactors = FALSE)

# Make the different severity level information a per-fire variable
sev_cat_prop <-
  fires_by_severity %>% 
  dplyr::select(fire_id, sev_cat, sev_cat_prop) %>% 
  tidyr::spread(key = sev_cat, value = sev_cat_prop, fill = 0) %>% 
  dplyr::rename(prop_unchanged = `0`,
                prop_low = `1`,
                prop_mod = `2`,
                prop_high = `3`)

# Make the different severity level information a per-fire variable
sev_cat_max_patch <-
  fires_by_severity %>% 
  dplyr::select(fire_id, sev_cat, max_patch_area_m2) %>% 
  tidyr::spread(key = sev_cat, value = max_patch_area_m2, fill = 0) %>% 
  dplyr::rename(max_patch_m2_unchanged = `0`,
                max_patch_m2_low = `1`,
                max_patch_m2_mod = `2`,
                max_patch_m2_high = `3`)

# Add a burn duration to the per-fire metadata information using the containment date 
fires <-
  md %>% 
  dplyr::mutate(alarm_date = ymd(paste(alarm_year, alarm_month, alarm_day, sep = "-"))) %>% 
  dplyr::mutate(cont_date = ymd(paste(cont_year, cont_month, cont_day, sep = "-"))) %>% 
  dplyr::mutate(burn_duration = as.numeric(cont_date - alarm_date)) %>% 
  left_join(fires_by_fire, by = "fire_id") %>% 
  dplyr::left_join(sev_cat_prop, by = "fire_id") %>% 
  dplyr::left_join(sev_cat_max_patch, by = "fire_id") %>% 
  dplyr::filter(!is.na(objective)) %>% 
  dplyr::mutate(objective = ifelse(objective == 1, yes = "suppression", no = "wfu"))

# Join the SDC data with the per-fire metadata + derived variables
fires_sdc <-
  fires %>% 
  left_join(sdc, by = "fire_id")

# Fix a few of the extreme outliers
# Pier fire was definitely not contained in 2 days. Contained on 2017-11-29, not 2017-08-30?
# 0000f67fc2d69fe5c384
fires_sdc$cont_month[fires_sdc$fire_id == "0000f67fc2d69fe5c384"] = 11
fires_sdc$cont_day[fires_sdc$fire_id == "0000f67fc2d69fe5c384"] = 29
fires_sdc$burn_duration[fires_sdc$fire_id == "0000f67fc2d69fe5c384"] = 92

# Cedar fire was definitely not contained the same day it started. Contained on 206-10-01
# 000098483297cad8ed1f
# http://cdfdata.fire.ca.gov/incidents/incidents_details_info?incident_id=1392
fires_sdc$cont_month[fires_sdc$fire_id == "000098483297cad8ed1f"] = 10
fires_sdc$cont_day[fires_sdc$fire_id == "000098483297cad8ed1f"] = 1
fires_sdc$burn_duration[fires_sdc$fire_id == "000098483297cad8ed1f"] = 46

# Round Fire in 2015 was 2015-02-06 to 2015-02-12; not just 2 days
# 0000b2e869b63ddf6a4f
# http://cdfdata.fire.ca.gov/incidents/incidents_details_info?incident_id=1073
fires_sdc$alarm_day[fires_sdc$fire_id == "0000b2e869b63ddf6a4f"] = 6
fires_sdc$cont_day[fires_sdc$fire_id == "0000b2e869b63ddf6a4f"] = 12
fires_sdc$burn_duration[fires_sdc$fire_id == "0000b2e869b63ddf6a4f"] = 6 

st_write(obj = fires_sdc, dsn = "data/data_output/event-level-severity-frap-metadata.csv")

