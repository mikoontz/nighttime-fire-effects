# Purpose: climate per active fire detection
# Conditional on there being an active fire detection, what is the climate?

library(tidyverse)
library(sf)
library(data.table)
library(tdigest)
library(lubridate)

get_mcd14mlGLDAS <- function(year, download = TRUE) {
  print(year)
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
  
  
  return(afd_thisYear)
}

afd <- lapply(2000:2019, get_mcd14mlGLDAS)

afd_filtered <- 
  afd[which(purrr::map_lgl(afd, .f = function(x) {return(!is.null(x) & !is.null(x$TYPE))}))] %>% 
  data.table::rbindlist()

afd_filtered <- afd_filtered[TYPE == 0 & CONFIDENCE > 10]

small <- afd[1:1000000, ]

afd_summary <-
  afd_filtered %>% 
  dplyr::mutate(acq_year = year(as.POSIXct(ACQ_DATE / 1000, origin = "1970-01-01", tz = "zulu")),
                acq_month = month(as.POSIXct(ACQ_DATE / 1000, origin = "1970-01-01", tz = "zulu")),
                acq_day = day(as.POSIXct(ACQ_DATE / 1000, origin = "1970-01-01", tz = "zulu")),
                acq_hour = floor(ACQ_TIME / 100),
                acq_min = ((ACQ_TIME / 100) - acq_hour) * 100,
                months_elapsed = ceiling(time_length(interval(ymd("2000-10-31"), as.Date(as.POSIXct(ACQ_DATE / 1000, origin = "1970-01-01", tz = "zulu"))), unit = "month"))) %>% 
  dplyr::group_by(months_elapsed, DAYNIGHT) %>% 
  dplyr::summarize(p10_vpd = tquantile(tdigest(vpd_hPa), probs = 0.1),
                   p50_vpd = tquantile(tdigest(vpd_hPa), probs = 0.5),
                   p90_vpd = tquantile(tdigest(vpd_hPa), probs = 0.9),
                   mean_vpd = mean(vpd_hPa, na.rm = TRUE))


par(mfrow = c(1, 2))
plot(afd_summary$months_elapsed, afd_summary$p50_vpd, type = "n", ylim = c(0, 60))
polygon(x = polygon.D.x, y = polygon.D.y, col = adjustcolor("red", alpha.f = 0.1))
polygon(x = polygon.N.x, y = polygon.N.y, col = adjustcolor("blue", alpha.f = 0.1))

lines(afd_summary$months_elapsed[afd_summary$DAYNIGHT == "D"], afd_summary$p50_vpd[afd_summary$DAYNIGHT == "D"], type = "l", col = "red", lwd = 3)
lines(afd_summary$months_elapsed[afd_summary$DAYNIGHT == "N"], afd_summary$p50_vpd[afd_summary$DAYNIGHT == "N"], type = "l", col = "blue", lwd = 3)

polygon.D.x <- c(afd_summary$months_elapsed[afd_summary$DAYNIGHT == "D"], rev(afd_summary$months_elapsed[afd_summary$DAYNIGHT == "D"]))
polygon.N.x <- c(afd_summary$months_elapsed[afd_summary$DAYNIGHT == "N"], rev(afd_summary$months_elapsed[afd_summary$DAYNIGHT == "N"]))

polygon.D.y <- c(afd_summary$p10_vpd[afd_summary$DAYNIGHT == "D"], rev(afd_summary$p90_vpd[afd_summary$DAYNIGHT == "D"]))
polygon.N.y <- c(afd_summary$p10_vpd[afd_summary$DAYNIGHT == "N"], rev(afd_summary$p90_vpd[afd_summary$DAYNIGHT == "N"]))

tail(afd_summary)

plot(afd_summary$months_elapsed, afd_summary$p90_vpd, pch = 19, col = factor(afd_summary$DAYNIGHT))
