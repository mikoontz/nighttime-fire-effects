# Purpose: merge the combined global MCD14ML/GLDAS2.1/RESOLVE ecoregion dataset with the global FIRED database.
# We want to have each active fire detection joined with what FIRED event it fell within the spatiotemporal window of (if any).

# Try to build this such that it can run on AWS.

# Load packages
library(tidyverse)
library(sf)
library(data.table)
library(lubridate)
library(purrr)
library(aws.s3)
library(furrr)
library(tictoc)
library(doParallel)
library(foreach)

# Download the western hemisphere FIRED data from S3 if it doesn't exist on disk
if(!file.exists(file.path("data", "data_raw", "FIRED", "western_hemisphere_to_may2019.gpkg"))) {
  dir.create(path = file.path("data", "data_raw", "FIRED"), showWarnings = FALSE, recursive = TRUE)
  fired_west <- save_object(object = "western_hemisphere_to_may2019.gpkg",
                            bucket = "earthlab-natem/modis-burned-area/delineated_events", 
                            file = file.path("data", "data_raw", "FIRED", "western_hemisphere_to_may2019.gpkg"))
  
} 

fired_west <- sf::st_read(file.path("data", "data_raw", "FIRED", "western_hemisphere_to_may2019.gpkg"))
fired_west <- 
  fired_west %>% 
  dplyr::mutate(start_year = year(start_date),
                start_month = month(start_date),
                start_day = day(start_date),
                last_year = year(last_date),
                last_month = month(last_date),
                last_day = day(last_date),
                duration = as.numeric(last_date - start_date))

fired_crs <- st_crs(fired_west)

determine_burning_days <- 
  function(start_date, last_date) {
    burning_days_df <- 
      format(seq(start_date,
                 last_date,
                 by = "day"), 
             format = "%Y-%m-%d") %>% 
      tibble() %>% 
      separate(col = ".", into = c("year", "month", "day"), sep = "-") %>% 
      dplyr::mutate(year = as.numeric(year),
                    month = as.numeric(month),
                    day = as.numeric(day))
    
    return(list(burning_days_df))
  }

v_determine_burning_days <- 
  Vectorize(determine_burning_days, vectorize.args = c("start_date", "last_date"))

fired <-
  fired_west %>%
  dplyr::filter(duration >= 0)

mcd14ml_years <- 2000:2019
mcd14ml_years_named <- mcd14ml_years %>% setNames(mcd14ml_years)

(start <- Sys.time())
cl <- makeCluster(4)
registerDoParallel(cl)

subset_idx <- 1:2

afd_list <- 
  foreach(i = mcd14ml_years_named[subset_idx], .packages = c("tidyverse", "data.table", "sf"), 
          .final = function(x) setNames(x, names(mcd14ml_years_named)[subset_idx])) %dopar% {
    
    fired_by_day <-
      fired %>%
      dplyr::filter(start_year == i) %>% 
      dplyr::mutate(burning_days = v_determine_burning_days(start_date, last_date)) %>% 
      tidyr::unnest(cols = burning_days)
    
    if(!file.exists(file.path("data", "data_output", "mcd14ml_gldas21_resolve-ecoregions", paste0("mcd14ml_gldas21_resolve-ecoregions_", i, ".csv")))) {
      download.file(url = "https://earthlab-mkoontz.s3-us-west-2.amazonaws.com/mcd14ml_gldas21_resolve-ecoregions/mcd14ml_gldas21_resolve-ecoregions_2000.csv",
                    destfile = file.path("data", "data_output", "mcd14ml_gldas21_resolve-ecoregions", paste0("mcd14ml_gldas21_resolve-ecoregions_", i, ".csv")))
    } 
    
    this_afd <- 
      data.table::fread(file.path("data", "data_output", "mcd14ml_gldas21_resolve-ecoregions", paste0("mcd14ml_gldas21_resolve-ecoregions_", i, ".csv"))) %>% 
      sf::st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326, remove = FALSE) %>% 
      sf::st_transform(fired_crs) %>% 
      dplyr::mutate(lon_sinu = sf::st_coordinates(.)[, 1],
                    lat_sinu = sf::st_coordinates(.)[, 2]) %>% 
      data.table::as.data.table()
    
    this_afd[, VERSION := as.character(VERSION)]
    
    this_afd_by_monthday_list <- split(x = this_afd, by = c("acq_month", "acq_day"), sorted = TRUE)
    x = names(this_afd_by_monthday_list)[273]
    
    spatiotemporal_match_fired_mcd14ml_list <-
      lapply(names(this_afd_by_monthday_list), FUN = function(x) {
        this_afd_name <- strsplit(x, split = "\\.")[[1]]
        this_afd_year <- as.numeric(i)
        this_afd_month <- as.numeric(this_afd_name[1])
        this_afd_day <- as.numeric(this_afd_name[2])
        
        this_afd_monthday <- this_afd_by_monthday_list[[x]]
        
        this_afd_sf <- 
          this_afd_monthday %>% 
          sf::st_as_sf()
        
        this_spatiotemporal_match_fired_mcd14ml <-
          fired_by_day %>% 
          dplyr::filter(year == this_afd_year & month == this_afd_month & day == this_afd_day) %>% 
          sf::st_intersection(this_afd_sf) %>% 
          sf::st_drop_geometry() %>%
          data.table::as.data.table()
        
        print(paste(this_afd_year, this_afd_month, this_afd_day, sep = "-"))
        
        return(this_spatiotemporal_match_fired_mcd14ml)
        
      })
    
   spatiotemporal_match_fired_mcd14ml <-
      data.table::rbindlist(spatiotemporal_match_fired_mcd14ml_list)
    
    return(spatiotemporal_match_fired_mcd14ml)
  }
stopCluster(cl)

# Add the TYPE column for the Near Real Time product
afd_list[["2019"]]$TYPE <- NA_integer_
afd_list[["2019"]] <- afd_list[["2019"]] %>% dplyr::select(names(afd_list[["2018"]]))

afd <- data.table::rbindlist(afd_list)
print(Sys.time() - start)

































# second attempt to use vectorized operations on FIRED perimeters ---------

# spatiotemporal_match_to_fired = function(id, start_year, start_month, start_day, last_year, last_month, last_day, geom) {
#   
#   afd_idx <- which(names(afd_list) %in% seq(start_year, last_year))
# 
#   if(length(afd_idx) == 1) {
#     afd <- afd_list[[afd_idx]]
#   }
# 
#   if(length(afd_idx) > 1) {
#     afd <-
#       afd_list[afd_idx] %>%
#       data.table::rbindlist(fill = TRUE)
#   }
#   
#   fired_afd_temporal_match <- afd[(acq_year >= start_year) & (acq_year <= last_year)
#                                   & (acq_month >= start_month) & (acq_month <= last_month)
#                                   & (acq_day >= start_day) & (acq_day <= last_day)]
#   
#   if(nrow(fired_afd_temporal_match) > 0) {
#     
#     fired_afd_spatiotemporal_match <-
#       fired_afd_temporal_match %>%
#       sf::st_as_sf() %>%
#       dplyr::filter(sf::st_intersects(., st_as_sf(st_geometry(geom), crs = fired_crs), sparse = FALSE)[, 1]) %>% 
#       sf::st_drop_geometry() %>%
#       data.table::as.data.table()
#     
#     if(nrow(fired_afd_spatiotemporal_match) == 0) {
#       fired_afd_spatiotemporal_match <- NA
#     }
#     
#   } else {fired_afd_spatiotemporal_match <- NA}
#   
#   rm(afd)
#   
#   return(list(fired_afd_spatiotemporal_match))
# }


# rowwise pmap solution ------------------------------------------------
# too slow methinks
# 
# tic()
# 
# afd_matches_to_fired <-
#   fired %>%
#   dplyr::filter(start_year == 2011) %>% 
#   dplyr::mutate(spatiotemporal_match = purrr::pmap(.l = list(id = id, 
#                                                              start_year = start_year,
#                                                              start_month = start_month,
#                                                              start_day = start_day,
#                                                              last_year = last_year,
#                                                              last_month = last_month,
#                                                              last_day = last_day,
#                                                              geom = geom), 
#                                                    .f = spatiotemporal_match_to_fired)) %>% 
#   dplyr::filter(!is.na(spatiotemporal_match)) %>%
#   tidyr::unnest(cols = spatiotemporal_match)
# 
# toc()


