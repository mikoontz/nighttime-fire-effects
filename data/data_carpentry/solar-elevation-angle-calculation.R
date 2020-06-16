# Add local time and solar elevation angle to all the MCD14ML active fire detections

# Purpose: climate per active fire detection
# Conditional on there being an active fire detection, what is the climate?

library(tidyverse)
library(data.table)
library(lubridate)
library(oce) # Not run, but checked against our roll-your-own method with good agreement
# The oce::sunAngle() method produces slightly more extreme sun angles than our method
# (i.e., sun higher than we predict during peak daytime and sun lower than we predict during
# peak nighttime)

get_mcd14ml <- function(year, download = FALSE) {
  
  if(file.exists(paste0("data/data_output/mcd14ml/mcd14ml_", year, ".csv"))) {
    (afd_thisYear <- data.table::fread(paste0("data/data_output/mcd14ml/mcd14ml_", year, ".csv")))
    
  } else {
    (afd_thisYear <- try(data.table::fread(paste0("https://earthlab-mkoontz.s3-us-west-2.amazonaws.com/mcd14ml/mcd14ml_", year, ".csv"))))
    
    if ("try-error" %in% class(afd_thisYear)) {
      afd_thisYear <- NULL
      
    } else {
      
      if (download) {
        if (!dir.exists("data/data_output/mcd14ml")) {
          dir.create("data/data_output/mcd14ml")
        }
        data.table::fwrite(x = afd_thisYear, 
                           file = paste0("data/data_output/mcd14ml/mcd14ml_", year, ".csv"))
      }
    }
  }
  
  print(year)
  
  return(afd_thisYear)
}

add_solar_elev_angle <- function(this_afd) {
  
  this_afd[, .geo := NULL]
  
  this_afd[, acq_hour := floor(ACQ_TIME / 100)]
  this_afd[, acq_min := ((ACQ_TIME / 100) - acq_hour) * 100]
  this_afd[, acq_datetime := as.POSIXct((ACQ_DATE / 1000) + (acq_hour * 3600) + (acq_min * 60), 
                                        origin = "1970-01-01", 
                                        tz = "America/Los_Angeles")]
  
  this_afd[, acq_datetime := lubridate::force_tz(acq_datetime, tzone = "UTC")]
  
  this_afd[, `:=`(acq_year = year(acq_datetime),
                  acq_month = month(acq_datetime),
                  acq_day = day(acq_datetime),
                  solar_offset = LONGITUDE / 15,
                  hemisphere = ifelse(LATITUDE >= 0, yes = "Northern hemisphere", no = "Southern hemisphere"))]
  
  this_afd[, acq_datetime_local := acq_datetime + as.duration(solar_offset * 60 * 60)]
  
  this_afd[, `:=`(local_doy = lubridate::yday(acq_datetime_local),
                  local_hour_decmin = ((acq_hour) + (acq_min / 60) + solar_offset + 24) %% 24)]
  
  this_afd[, `:=`(local_solar_hour_decmin_round = round(local_hour_decmin),
                  local_solar_hour_decmin_round0.5 = round(local_hour_decmin * 2) / 2)]
  
  # https://en.wikipedia.org/wiki/Solar_zenith_angle
  # https://en.wikipedia.org/wiki/Position_of_the_Sun#Declination_of_the_Sun_as_seen_from_Earth
  # https://en.wikipedia.org/wiki/Hour_angle
  
  this_afd[, `:=`(h = (local_hour_decmin - 12) * 15 * pi / 180,
                  phi = LATITUDE * pi / 180,
                  delta = -asin(0.39779 * cos(pi / 180 * (0.98565 * (local_doy + 10) + 360 / pi * 0.0167 * sin(pi / 180 * (0.98565 * (local_doy - 2)))))))]
  
  this_afd[, solar_elev_ang := (asin(sin(phi)*sin(delta) + cos(phi)*cos(delta)*cos(h))) * 180 / pi]
  
  # afd[, solar_elev_ang2 := oce::sunAngle(t = acq_datetime,
  #                                        longitude = LONGITUDE, 
  #                                        latitude = LATITUDE, 
  #                                        useRefraction = FALSE)$altitude]

    this_year <- as.numeric(this_afd[1, "acq_year"])
  
  print(this_year)
  
  return(this_afd)
}

write_afd_solar_elev_angle <- function(this_afd) {
  
  this_year <- as.numeric(this_afd[1, "acq_year"])
  
  fwrite(this_afd, paste0("data/data_output/mcd14ml_solar-elevation-angle/mcd14ml_solar-elevation-angle_", this_year, ".csv"))
  
  print(this_year)
}

afd <- lapply(2001:2019, get_mcd14ml)

lapply(afd, add_solar_elev_angle)

dir.create('data/data_output/mcd14ml_solar-elevation-angle')

lapply(afd, write_afd_solar_elev_angle)
