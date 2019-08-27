library(tidyverse)
library(sf)
library(data.table)
library(tdigest)
library(lubridate)

get_FIREDgldas2.1 <- function(year, download = TRUE) {
  
  if(file.exists(paste0("data/data_output/FIRED-with-gldas2.1-climate-variables/FIRED-climate-variables_", year, ".csv"))) {
    (FIRED_climateVars_thisYear <- 
       data.table::fread(paste0("data/data_output/FIRED-with-gldas2.1-climate-variables/FIRED-climate-variables_", year, ".csv")))
    
  } else {
    (FIRED_climateVars_thisYear <- try(data.table::fread(paste0("https://earthlab-mkoontz.s3-us-west-2.amazonaws.com/FIRED-with-gldas2.1-climate-variables/FIRED-climate-variables_", year, ".csv"))))
    
    if ("try-error" %in% class(FIRED_climateVars_thisYear)) {
      FIRED_climateVars_thisYear <- NULL
      
    } else {
      
      if (download) {
        if (!dir.exists("data/data_output/FIRED-with-gldas2.1-climate-variables")) {
          dir.create("data/data_output/FIRED-with-gldas2.1-climate-variables")
        }
        data.table::fwrite(x = FIRED_climateVars_thisYear, 
                           file = paste0("data/data_output/FIRED-with-gldas2.1-climate-variables/FIRED-climate-variables_", year, ".csv"))
      }
    }
  }
  
  print(year)
  
  return(FIRED_climateVars_thisYear)
}

get_FIREDmetadata <- function(download = TRUE) {
  
  if(file.exists("data/data_output/FIRED-with-gldas2.1-climate-variables/ee-FIRED_metadata.csv")) {
    (FIRED_metadata <- 
       data.table::fread("data/data_output/FIRED-with-gldas2.1-climate-variables/ee-FIRED_metadata.csv"))
    
  } else {
    (FIRED_metadata <- try(data.table::fread("https://earthlab-mkoontz.s3-us-west-2.amazonaws.com/FIRED-with-gldas2.1-climate-variables/ee-FIRED_metadata.csv")))
    
    if ("try-error" %in% class(FIRED_metadata)) {
      FIRED_metadata <- NULL
      
    } else {
      
      if (download) {
        if (!dir.exists("data/data_output/FIRED-with-gldas2.1-climate-variables")) {
          dir.create("data/data_output/FIRED-with-gldas2.1-climate-variables")
        }
        data.table::fwrite(x = FIRED_metadata, 
                           file = "data/data_output/FIRED-with-gldas2.1-climate-variables/ee-FIRED_metadata.csv")
      }
    }
  }
  
  return(FIRED_metadata)
}

FIRED <- lapply(2000:2019, get_FIREDgldas2.1)

FIRED_filtered <- 
  FIRED[which(purrr::map_lgl(FIRED, .f = function(x) {return(!is.null(x))}))] %>% 
  data.table::rbindlist() %>% 
  dplyr::mutate(datetime = as.POSIXct(gldas_datetime / 1000,
                                      origin = "1970-01-01", 
                                      tz = "zulu"),
                year = year(datetime),
                month = month(datetime),
                day = day(datetime),
                hour = hour(datetime),
                minute = minute(datetime)) %>% 
  tidyr::separate(col = 'system:index', into = c("FIRED.system.index",
                                               "GLDAS2.1.system.index",
                                               "GLDAS2.1.timestamp")) %>% 
  tidyr::unite(col = GLDAS2.1.system.index, GLDAS2.1.system.index, GLDAS2.1.timestamp)

FIRED_meta <- get_FIREDmetadata()
FIRED_meta <-
  FIRED_meta %>% 
  dplyr::select(-.geo) %>% 
  dplyr::rename(FIRED.system.index = 'system:index')

FIRED_full <- dplyr::left_join(FIRED_filtered, FIRED_meta, by = "FIRED.system.index")