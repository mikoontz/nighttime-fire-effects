# Purpose: turn mcd14ml + gldas2.1 climate data product into a shapefile for upload to Earth Engine
# File was ~94GB. Probably best to do this by year
# Be sure to deal with the int64 issue for ACQ_DATE. This isn't an acceptable format for .shp.

library(tidyverse)
library(sf)
library(data.table)
library(tdigest)
library(lubridate)

if(!file.exists("data/data_output/mcd14ml-with-gldas2.1-climate-variables/mcd14ml_with_gldas_climate_variables.csv")) {
  source("data/data_carpentry/merge_mcd14ml_with_gldas2.1.R")
}


make_ee_ready <- function(year) {
  
  DT <- data.table::fread(file.path("data", "data_output", "mcd14ml-with-gldas2.1-climate-variables", paste0("mcd14ml_with_gldas_climate_variables_", year, ".csv")))
  
  DT[, .geo := NULL]
  DT[, acq_hour := floor(ACQ_TIME / 100)]
  DT[, acq_min := ((ACQ_TIME / 100) - acq_hour) * 100]
  DT[, acq_datetime := as.POSIXct((ACQ_DATE / 1000) + (acq_hour * 3600) + (acq_min * 60), 
                                  origin = "1970-01-01", 
                                  tz = "America/Los_Angeles")]
  
  DT[, `:=`(acq_year = year(acq_datetime),
            acq_month = month(acq_datetime),
            acq_day = day(acq_datetime),
            solar_offset = LONGITUDE / 15)]
  
  DT[, acq_datetime_local := acq_datetime + as.duration(solar_offset * 60 * 60)]
  
  DT[, `:=`(loc_doy = lubridate::yday(acq_datetime_local),
            loc_hr_dec = ((acq_hour) + (acq_min / 60) + solar_offset + 24) %% 24,
            loc_year = year(acq_datetime_local),
            loc_month = month(acq_datetime_local),
            loc_day = day(acq_datetime_local),
            loc_hour = hour(acq_datetime_local),
            loc_min = minute(acq_datetime_local)
            )]
  
  DT[, ACQ_DATE := ymd(paste(acq_year, acq_month, acq_day, sep = "-"))]
  DT[, LOC_DATE := ymd(paste(loc_year, loc_month, loc_day, sep = "-"))]
  
  DT[, `:=`(acq_datetime = NULL,
            acq_datetime_local = NULL,
            solar_offset = NULL)]
  
  DT <- rename(DT, 
               ee_index = "system:index",
               Psurf_f_i = Psurf_f_inst,
               Qair_f_i = Qair_f_inst,
               SWdown_f_t = SWdown_f_tavg,
               SM_0_10_i = SoilMoi0_10cm_inst,
               Swnet_t = Swnet_tavg,
               Wind_f_i = Wind_f_inst,
               Tair_f_i = Tair_f_inst)
  
  
  DT_sf <- st_as_sf(DT, coords = c("LONGITUDE", "LATITUDE"), crs = 4326, remove = FALSE)
  
  dir.create(file.path("data", "data_output", "mcd14ml-with-gldas2.1-climate-variables", paste0("mcd14ml-gldas2.1_", year)))
  
  target_filepath <- file.path("data", "data_output", "mcd14ml-with-gldas2.1-climate-variables", paste0("mcd14ml-gldas2.1_", year), paste0("mcd14ml-gldas2.1_", year, ".shp"))
  
  st_write(DT_sf, target_filepath, delete_dsn = TRUE)
  
};

lapply(years, make_ee_ready)
