library(tidyverse)
library(sf)
library(data.table)
library(tdigest)
library(lubridate)

get_FIREDnldas2 <- function(year, download = TRUE) {
  
  if(file.exists(paste0("data/data_output/FIRED-with-nldas2-climate-variables_CONUS/FIRED-nldas2-climate-variables_CONUS_", year, ".csv"))) {
    (FIRED_climateVars_thisYear <- 
       data.table::fread(paste0("data/data_output/FIRED-with-nldas2-climate-variables_CONUS/FIRED-nldas2-climate-variables_CONUS_", year, ".csv")))
    
  } else {
    (FIRED_climateVars_thisYear <- try(data.table::fread(paste0("https://earthlab-mkoontz.s3-us-west-2.amazonaws.com/FIRED-with-nldas2-climate-variables_CONUS/FIRED-nldas2-climate-variables_CONUS_", year, ".csv"))))
    
    if ("try-error" %in% class(FIRED_climateVars_thisYear)) {
      FIRED_climateVars_thisYear <- NULL
      
    } else {
      
      if (download) {
        if (!dir.exists("data/data_output/FIRED-with-nldas2-climate-variables_CONUS")) {
          dir.create("data/data_output/FIRED-with-nldas2-climate-variables_CONUS")
        }
        data.table::fwrite(x = FIRED_climateVars_thisYear, 
                           file = paste0("data/data_output/FIRED-with-nldas2-climate-variables_CONUS/FIRED-nldas2-climate-variables_CONUS_", year, ".csv"))
      }
    }
  }
  
  print(year)
  
  return(FIRED_climateVars_thisYear)
}

get_FIREDmetadata <- function(download = TRUE) {
  
  if(file.exists("data/data_output/FIRED-with-nldas2-climate-variables_CONUS/ee-FIRED_CONUS_metadata.gpkg")) {
    (FIRED_metadata <- 
       sf::st_read("data/data_output/FIRED-with-nldas2-climate-variables_CONUS/ee-FIRED_CONUS_metadata.gpkg", stringsAsFactors = FALSE))
    
  } else {
    (FIRED_metadata <- try(sf::st_read("https://earthlab-mkoontz.s3-us-west-2.amazonaws.com/FIRED-with-nldas2-climate-variables_CONUS/ee-FIRED_CONUS_metadata.gpkg", stringsAsFactors = FALSE)))
    
    if ("try-error" %in% class(FIRED_metadata)) {
      FIRED_metadata <- NULL
      
    } else {
      
      if (download) {
        if (!dir.exists("data/data_output/FIRED-with-nldas2-climate-variables_CONUS")) {
          dir.create("data/data_output/FIRED-with-nldas2-climate-variables_CONUS")
        }
        sf::st_write(obj = FIRED_metadata, 
                     dsn = "data/data_output/FIRED-with-nldas2-climate-variables_CONUS/ee-FIRED_CONUS_metadata.gpkg")
      }
    }
  }
  
  return(FIRED_metadata)
}

FIRED <- lapply(2000:2019, get_FIREDnldas2)

FIRED_filtered <- 
  FIRED[which(purrr::map_lgl(FIRED, .f = function(x) {return(!is.null(x))}))] %>% 
  data.table::rbindlist() %>% 
  dplyr::mutate(datetime = as.POSIXct(nldas_datetime / 1000,
                                      origin = "1970-01-01", 
                                      tz = "zulu"),
                year = year(datetime),
                month = month(datetime),
                day = day(datetime),
                hour = hour(datetime),
                minute = minute(datetime)) %>% 
  tidyr::separate(col = 'system:index', into = c("FIRED.system.index",
                                                 "NLDAS2.system.index",
                                                 "NLDAS2.timestamp")) %>% 
  tidyr::unite(col = NLDAS2.system.index, NLDAS2.system.index, NLDAS2.timestamp)

FIRED_meta <- get_FIREDmetadata()

FIRED_meta <-
  FIRED_meta %>% 
  dplyr::rename(FIRED.system.index = 'system.index')

FIRED_nldas <- dplyr::left_join(FIRED_filtered, FIRED_meta, by = "FIRED.system.index")
