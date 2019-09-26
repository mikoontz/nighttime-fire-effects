# Purpose: investigate missing GLDAS2.1 data from the MCD14ML detections that got an NA

# only 2003, 2006, 2007, 2009, 2014, and 2017 have missing GLDAS2.1 data, which is weird.

library(tidyverse)
library(sf)
library(data.table)
library(lubridate)

missing_meta <- read_csv("data/data_output/n-records_active-fire-detections.csv")

missing_meta %>% select(starts_with("nrecords"))
missing_meta %>% select(starts_with("records_lost"))

missing_gldas21_meta <- 
  read_csv("data/data_output/n-records_active-fire-detections.csv") %>% 
  filter(records_lost_gldas21 > 0) %>% select(year, path_mcd14ml, path_mcd14ml_gldas21)

missing_gldas21_resolve_meta <- 
  read_csv("data/data_output/n-records_active-fire-detections.csv") %>% 
  filter(records_lost_gldas21_resolve > 0) %>% select(year, path_mcd14ml_gldas21, path_mcd14ml_gldas21_resolve)

missing_mcd14ml <- 
  lapply(missing_gldas21_meta$path_mcd14ml, 
         FUN = function(x) {
           this_afd <- st_read(x, stringsAsFactors = FALSE) %>% st_drop_geometry()
           return(as.data.table(this_afd))
         })

missing_mcd14ml_gldas21 <- lapply(missing_gldas21_meta$path_mcd14ml_gldas21, data.table::fread)

missing_mcd14ml_dt <- 
  data.table::rbindlist(missing_mcd14ml)

missing_mcd14ml_gldas21_dt <-
  data.table::rbindlist(missing_mcd14ml_gldas21)

missing_mcd14ml_gldas21_dt[, .geo := NULL]
missing_mcd14ml_gldas21_dt[, acq_hour := floor(ACQ_TIME / 100)]
missing_mcd14ml_gldas21_dt[, acq_min := ((ACQ_TIME / 100) - acq_hour) * 100]
missing_mcd14ml_gldas21_dt[, acq_datetime := as.POSIXct((ACQ_DATE / 1000) + (acq_hour * 3600) + (acq_min * 60), 
                                                     origin = "1970-01-01", 
                                                     tz = "America/Los_Angeles")]

missing_mcd14ml_gldas21_dt[, `:=`(acq_year = year(acq_datetime),
                               acq_month = month(acq_datetime),
                               acq_day = day(acq_datetime))]

missing_mcd14ml_gldas21_dt[, ACQ_DATE := ymd(paste(acq_year, acq_month, acq_day, sep = "-"))]
missing_mcd14ml_gldas21_dt[, `:=`(ACQ_TIME = as.character(ACQ_TIME),
                                  VERSION = as.character(VERSION))]

lost_records_gldas21 <- missing_mcd14ml_dt %>% anti_join(missing_mcd14ml_gldas21_dt)

