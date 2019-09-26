# Purpose: count the number of active fire detections that are left out due to the GLDAS2.1 product not covering
# the coast very well.

library(tidyverse)
library(sf)
library(rgdal)
library(lubridate)
library(data.table)

# just mcd14ml --------------------------------------------------------

original_mcd14ml_files <- list.files(path = "data/data_raw/fire_archive_M6", pattern = ".shp", recursive = TRUE, full.names = TRUE)

original_mcd14ml <- lapply(original_mcd14ml_files, function(path) {
  
  this_mcd14ml <- st_read(path)
  acq_date_year <- year(ymd(this_mcd14ml[1, ]$ACQ_DATE))
  
  return(tibble(path = path, year = acq_date_year, nrecords_original_mcd14ml = nrow(this_mcd14ml)))
})

original_mcd14ml_meta <- 
  bind_rows(original_mcd14ml) %>% 
  dplyr::arrange(year) %>% 
  dplyr::rename(path_mcd14ml = path,
                nrecords_mcd14ml = nrecords_original_mcd14ml)


# mcd14ml uploaded to EE --------------------------------------------------

mcd14ml_ee_files <- list.files(path = "data/data_output/mcd14ml", pattern = "csv", recursive = TRUE, full.names =  TRUE)

mcd14ml_ee <- sapply(mcd14ml_ee_files, function(path) {
  print(path)
  return(nrow(data.table::fread(path)))
})

sum(mcd14ml_ee)
# 82767722 MCD14ML records uploaded to EE

mcd14ml_ee_meta <-
  tibble(path = names(mcd14ml_ee),
         nrecords_mcd14ml_ee = mcd14ml_ee) %>% 
  dplyr::mutate(year = as.numeric(substr(path, start = nchar(path) - 7, stop = nchar(path) - 4))) %>% 
  dplyr::rename(path_mcd14ml_ee = path)

# mcd14ml plus gldas2.1 ------------------------------------------------

mcd14ml_gldas2.1_files <- list.files(path = "data/data_output/mcd14ml_gldas21", pattern = "csv", recursive = TRUE, full.names =  TRUE)

mcd14ml_gldas2.1 <- sapply(mcd14ml_gldas2.1_files, function(path) {
  print(path)
  return(nrow(data.table::fread(path)))
})

sum(mcd14ml_gldas2.1)
# 82586182 MCD14ML records that we could join the GLDAS2.1 climate data to

mcd14ml_gldas2.1_meta <-
  tibble(path = names(mcd14ml_gldas2.1),
         nrecords_mcd14ml_gldas21 = mcd14ml_gldas2.1) %>% 
  dplyr::mutate(year = as.numeric(substr(path, start = nchar(path) - 7, stop = nchar(path) - 4))) %>% 
  dplyr::rename(path_mcd14ml_gldas21 = path)


# mcd14ml plus gldas2.1 plus resolve ------------------------------------------------

mcd14ml_gldas2.1_resolve_files <- list.files(path = "data/data_output/mcd14ml_gldas21_resolve-ecoregions", pattern = "csv", recursive = TRUE, full.names =  TRUE)

mcd14ml_gldas2.1_resolve <- sapply(mcd14ml_gldas2.1_resolve_files, function(path) {
  print(path)
  return(nrow(data.table::fread(path)))
})

sum(mcd14ml_gldas2.1_resolve)
# 82586182 MCD14ML records that we could join the GLDAS2.1 climate data to

mcd14ml_gldas2.1_resolve_meta <-
  tibble(path = names(mcd14ml_gldas2.1_resolve),
         nrecords_mcd14ml_gldas21_resolve = mcd14ml_gldas2.1_resolve) %>% 
  dplyr::mutate(year = as.numeric(substr(path, start = nchar(path) - 7, stop = nchar(path) - 4))) %>% 
  dplyr::rename(path_mcd14ml_gldas21_resolve = path)


# sum(sapply(original_mcd14ml, nrow))
# 82767722 records in the original fire_archive_M6_xxxxx detections

# percent of data excluded ~0.22%
1 - (82586182 / 82767722)

# join of the mcd14ml/gldas21 and mcd14ml/gldas21/resolve metadata
  
active_fire_detections_by_database_year <- 
  original_mcd14ml_meta %>% 
  left_join(mcd14ml_ee_meta, by = "year") %>% 
  left_join(mcd14ml_gldas2.1_meta, by = "year") %>% 
  left_join(mcd14ml_gldas2.1_resolve_meta, by = "year") %>% 
  dplyr::select(year, starts_with("path"), everything()) %>%
  dplyr::mutate(records_lost_ee = nrecords_mcd14ml - nrecords_mcd14ml_ee,
                records_lost_gldas21 = nrecords_mcd14ml - nrecords_mcd14ml_gldas21,
                records_lost_gldas21_resolve = nrecords_mcd14ml_gldas21 - nrecords_mcd14ml_gldas21_resolve,
                records_lost_total = nrecords_mcd14ml - nrecords_mcd14ml_gldas21_resolve)

active_fire_detections_by_database_year

write_csv(x = active_fire_detections_by_database_year, path = "data/data_output/n-records_active-fire-detections.csv")
