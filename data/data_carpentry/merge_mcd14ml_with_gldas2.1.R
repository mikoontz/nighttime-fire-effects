# Purpose: climate per active fire detection
# Conditional on there being an active fire detection, what is the climate?

library(tidyverse)
library(sf)
library(data.table)
library(tdigest)
library(lubridate)
library(tmap)
library(mgcv)
library(gganimate)
library(viridis)

get_mcd14mlGLDAS <- function(year, download = TRUE) {
  
  if(file.exists(paste0("data/data_output/mcd14ml-with-gldas2.1-climate-variables/mcd14ml_with_gldas_climate_variables_", year, ".csv"))) {
    (afd_thisYear <- data.table::fread(paste0("data/data_output/mcd14ml-with-gldas2.1-climate-variables/mcd14ml_with_gldas_climate_variables_", year, ".csv")))
    
  } else {
    (afd_thisYear <- try(data.table::fread(paste0("https://earthlab-mkoontz.s3-us-west-2.amazonaws.com/mcd14ml-with-gldas2.1-climate-variables/mcd14ml_with_gldas_climate_variables_", year, ".csv"))))
    
    if ("try-error" %in% class(afd_thisYear)) {
      afd_thisYear <- NULL
      
    } else {
      
      if (download) {
        if (!dir.exists("data/data_output/mcd14ml-with-gldas2.1-climate-variables")) {
          dir.create("data/data_output/mcd14ml-with-gldas2.1-climate-variables")
        }
        data.table::fwrite(x = afd_thisYear, 
                           file = paste0("data/data_output/mcd14ml-with-gldas2.1-climate-variables/mcd14ml_with_gldas_climate_variables_", year, ".csv"))
      }
    }
  }
  
  print(year)
  
  return(afd_thisYear)
}

afd <- lapply(2000:2019, get_mcd14mlGLDAS)

afd_filtered <- 
  afd[which(purrr::map_lgl(afd, .f = function(x) {return(!is.null(x) & !is.null(x$TYPE))}))] %>% 
  data.table::rbindlist()

afd_filtered <- afd_filtered[TYPE == 0 & CONFIDENCE > 10]
afd_filtered[, .geo := NULL]

afd_filtered[, acq_hour := floor(ACQ_TIME / 100)]
afd_filtered[, acq_min := ((ACQ_TIME / 100) - acq_hour) * 100]
afd_filtered[, acq_datetime := as.POSIXct((ACQ_DATE / 1000) + (acq_hour * 3600) + (acq_min * 60), 
                                          origin = "1970-01-01", 
                                          tz = "America/Los_Angeles")]

afd_filtered[, `:=`(acq_year = year(acq_datetime),
                    acq_month = month(acq_datetime),
                    acq_day = day(acq_datetime),
                    solar_offset = LONGITUDE / 15,
                    hemisphere = ifelse(LATITUDE >= 0, yes = "Northern hemisphere", no = "Southern hemisphere"))]

afd_filtered[, acq_datetime_local := acq_datetime + as.duration(solar_offset * 60 * 60)]

afd_filtered[, `:=`(local_doy = lubridate::yday(acq_datetime_local),
                    local_hour_decmin = ((acq_hour) + (acq_min / 60) + solar_offset + 24) %% 24)]

afd_filtered[, `:=`(local_solar_hour_decmin_round = round(local_hour_decmin),
                    local_solar_hour_decmin_round0.5 = round(local_hour_decmin * 2) / 2)]

# https://en.wikipedia.org/wiki/Solar_zenith_angle
# https://en.wikipedia.org/wiki/Position_of_the_Sun#Declination_of_the_Sun_as_seen_from_Earth
# https://en.wikipedia.org/wiki/Hour_angle

afd_filtered[, `:=`(h = (local_hour_decmin - 12) * 15 * pi / 180,
                    phi = LATITUDE * pi / 180,
                    delta = -asin(0.39779 * cos(pi / 180 * (0.98565 * (local_doy + 10) + 360 / pi * 0.0167 * sin(pi / 180 * (0.98565 * (local_doy - 2)))))))]

afd_filtered[, solar_elev_ang := (asin(sin(phi)*sin(delta) + cos(phi)*cos(delta)*cos(h))) * 180 / pi]

afd_filtered[, .(min_solar_ang = min(solar_elev_ang)), by = (DAYNIGHT)]
afd_filtered[, .(max_solar_ang = max(solar_elev_ang)), by = (DAYNIGHT)]
afd_filtered[, .(pct_solar_ang_gt_0 = length(which(solar_elev_ang > 0)) / length(solar_elev_ang)), by = (DAYNIGHT)]
afd_filtered[, .(pct_solar_ang_lt_0 = length(which(solar_elev_ang < 0)) / length(solar_elev_ang)), by = (DAYNIGHT)]

fwrite("data/data_output/mcd14ml-with-gldas2.1-climate-variables/mcd14ml_with_gldas_climate_variables.csv")
