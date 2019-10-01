# Purpose: merge the combined global MCD14ML/GLDAS2.1/RESOLVE ecoregion dataset with the global FIRED database.
# We want to have each active fire detection joined with what FIRED event it fell within the spatiotemporal window of (if any).

# Try to build this such that it can run on AWS.

library(tidyverse)
library(sf)
library(data.table)
library(lubridate)
library(purrr)
library(aws.s3)
library(furrr)
library(tictoc)



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
                last_day = day(last_date))

fired_crs <- st_crs(fired_west)

mcd14ml_years <- 2000:2019
mcd14ml_years_named <- mcd14ml_years %>% setNames(mcd14ml_years)
i = 2000

(start <- Sys.time())
afd_list <- 
  lapply(X = mcd14ml_years_named, FUN = function(i) {
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
    
    this_afd[, VERSION := as.numeric(VERSION)]
    
    return(this_afd)
  })

# Add the TYPE column for the Near Real Time product
afd_list[["2019"]]$TYPE <- NA_integer_
afd_list[["2019"]] <- afd_list[["2019"]] %>% dplyr::select(names(afd_list[["2018"]]))

print(Sys.time() - start)

# second attempt to use vectorized operations on FIRED perimeters ---------
spatiotemporal_match_to_fired = function(id, start_year, start_month, start_day, last_year, last_month, last_day, geom) {
  
  afd_idx <- which(names(afd_list) %in% seq(start_year, last_year))
  
  if(length(afd_idx) == 1) {
    afd <- afd_list[[afd_idx]]
  }
  
  if(length(afd_idx) > 1) {
    afd <-
      afd_list[afd_idx] %>% 
      data.table::rbindlist(fill = TRUE)
  }
  
  fired_afd_temporal_match <- afd[(acq_year >= start_year) & (acq_year <= last_year)
                                  & (acq_month >= start_month) & (acq_month <= last_month)
                                  & (acq_day >= start_day) & (acq_day <= last_day)]
  
  if(nrow(fired_afd_temporal_match) > 0) {
    
    fired_afd_spatiotemporal_match <-
      fired_afd_temporal_match %>%
      sf::st_as_sf() %>%
      dplyr::filter(sf::st_intersects(., st_as_sf(st_geometry(geom), crs = fired_crs), sparse = FALSE)[, 1]) %>% 
      sf::st_drop_geometry() %>%
      data.table::as.data.table()
    
    if(nrow(fired_afd_spatiotemporal_match) == 0) {
      fired_afd_spatiotemporal_match <- NA
    }
    
  } else {fired_afd_spatiotemporal_match <- NA}
  
  rm(afd)
  
  return(fired_afd_spatiotemporal_match)
}

public_ip <- c("34.223.88.162", "34.222.183.90")

# This is where my pem file lives
ssh_private_key_file <- file.path("H:", "dev", "aws", "mkoontz.pem")


# connect to EC2 instances ------------------------------------------------

cl <- makeClusterPSOCK(
  
  # Public IP number of EC2 instance
  workers = public_ip,
  
  # User name (always 'ubuntu')
  user = "ubuntu",
  
  # Use private SSH key registered with AWS
  rshopts = c(
    "-o", "StrictHostKeyChecking=no",
    "-o", "IdentitiesOnly=yes",
    "-i", ssh_private_key_file
  ),
  
  # Set up .libPaths() for the 'ubuntu' user and
  # install furrr
  rscript_args = c(
    "-e", shQuote("local({p <- Sys.getenv('R_LIBS_USER'); dir.create(p, recursive = TRUE, showWarnings = FALSE); .libPaths(p)})"),
    "-e", shQuote("install.packages('furrr')")
  ),
  
  # Switch this to TRUE to see the code that is run on the workers without
  # making the connection
  dryrun = FALSE
)

cl

tic()

afd_matches_to_fired <-
  fired_west %>%
  slice(1:1000) %>% 
  dplyr::mutate(spatiotemporal_match = purrr::pmap(.l = list(id = id, 
                                                             start_year = start_year,
                                                             start_month = start_month,
                                                             start_day = start_day,
                                                             last_year = last_year,
                                                             last_month = last_month,
                                                             last_day = last_day,
                                                             geom = geom), 
                                                   .f = spatiotemporal_match_to_fired)) %>% 
  dplyr::filter(!is.na(spatiotemporal_match)) %>%
  tidyr::unnest(cols = spatiotemporal_match)

toc()


















# test_idx = 10
# id = fired_west$id[test_idx]
# start_year = fired_west$start_year[test_idx]
# start_month = fired_west$start_month[test_idx]
# start_day = fired_west$start_day[test_idx]
# last_day = fired_west$last_day[test_idx]
# last_month = fired_west$last_month[test_idx]
# last_year = fired_west$last_year[test_idx]
# geom = fired_west$geom[test_idx]

# # Naive attempt at spatiotemporal join by iterating through each F --------
# fired <- fired_west[1:100, ]
# 
# (start <- Sys.time())
# fired_afd_spatiotemporal_match_list <-
#   purrr::pmap(.l = list(id = fired$id, 
#                         start_year = fired$start_year,
#                         start_month = fired$start_month,
#                         start_day = fired$start_day,
#                         last_year = fired$last_year,
#                         last_month = fired$last_month,
#                         last_day = fired$last_day,
#                         geom = fired$geom), 
#               .f = function(id, start_year, start_month, start_day, last_year, last_month, last_day, geom) {
#                 
#                 afd_idx <- which(names(afd_list) %in% seq(start_year, last_year))
#                 
#                 if(length(afd_idx) == 1) {
#                   afd <- afd_list[[afd_idx]]
#                 }
#                 
#                 if(length(afd_idx) > 1) {
#                   afd <-
#                     afd_list[afd_idx] %>% 
#                     data.table::rbindlist(fill = TRUE)
#                 }
#                 
#                 fired_afd_temporal_match <- afd[(acq_year >= start_year) & (acq_year <= last_year)
#                                                 & (acq_month >= start_month) & (acq_month <= last_month)
#                                                 & (acq_day >= start_day) & (acq_day <= last_day)]
#                 
#                 if(nrow(fired_afd_temporal_match) > 0) {
#                   this_fired_sf <- st_as_sf(tibble(id, start_year, start_month, start_day, last_year, last_month, last_day, geom = st_geometry(geom)), crs = fired_crs)
#                   
#                   fired_afd_spatiotemporal_match <-
#                     fired_afd_temporal_match %>%
#                     sf::st_as_sf() %>%
#                     sf::st_intersection(this_fired_sf) %>%
#                     sf::st_drop_geometry() %>%
#                     data.table::as.data.table()
#                   
#                   if(nrow(fired_afd_spatiotemporal_match) == 0) {
#                     fired_afd_spatiotemporal_match <- NULL
#                   }
#                   
#                 } else {fired_afd_spatiotemporal_match <- NULL}
#                 
#                 rm(afd)
#                 
#                 return(fired_afd_spatiotemporal_match)
#               })
# 
# fired_matches <- 
#   fired_afd_spatiotemporal_match_list %>% 
#   purrr::compact() %>% 
#   rbindlist()
# 
# (Sys.time() - start)
# 
# 
# 
# # third attempt that tries to use data.table -----------------------------
# 
# # Naive attempt at spatiotemporal join by iterating through each F --------
# afd_small <- afd_list[[1]][1:100, ]
# 
# (start <- Sys.time())
# 
# spatiotemporal_match_to_fired <- function(acq_year, acq_month, acq_day, lon_sinu, lat_sinu) {
#   
#   fired_temporal_match <- 
#     fired_west %>% 
#     dplyr::filter(start_year <= acq_year & last_year >= acq_year & start_month <= acq_month & last_month >= acq_month & start_day <= acq_day & last_day >= acq_day) 
#   # %>% 
#   #   sf::st_intersection(st_as_sf(Vectorize(st_point(c(lon_sinu, lat_sinu))), crs = fired_crs))
#   
#   return(list(fired_temporal_match))
# }
# 
# DT <- as.data.table(afd_small)
# 
# test <-
#   DT[, spatiotemporal_match := list(fired_west[(fired_west$start_year <= acq_year) & (fired_west$last_year >= acq_year) 
#                                                & (fired_west$start_month <= acq_month) & (fired_west$last_month >= acq_month) 
#                                                & (fired_west$start_day <= acq_day) & (fired_west$last_day >= acq_day), ])]
# %>% 
#   dplyr::mutate(spatiotemporal_match = list(fired_west[(fired_west$start_year <= acq_year) & (fired_west$last_year >= acq_year) 
#                                                        & (fired_west$start_month <= acq_month) & (fired_west$last_month >= acq_month) 
#                                                        & (fired_west$start_day <= acq_day) & (fired_west$last_day >= acq_day), ]))
# test %>% nest(cols = c(spatiotemporal_match = spatiotemporal_match))
# 
# test <-
#   afd_small %>% 
#   dplyr::mutate(spatiotemporal_match = list(fired_west[(fired_west$start_year <= acq_year) & (fired_west$last_year >= acq_year) 
#                                                        & (fired_west$start_month <= acq_month) & (fired_west$last_month >= acq_month) 
#                                                        & (fired_west$start_day <= acq_day) & (fired_west$last_day >= acq_day), ]))
# 
# 
# # fired_afd_spatiotemporal_match_list <-
# #   purrr::pmap(.l = list(id = fired$id, 
# #                         start_year = fired$start_year,
# #                         start_month = fired$start_month,
# #                         start_day = fired$start_day,
# #                         last_year = fired$last_year,
# #                         last_month = fired$last_month,
# #                         last_day = fired$last_day,
# #                         geom = fired$geom), 
# #               .f = function(id, start_year, start_month, start_day, last_year, last_month, last_day, geom) {
# #                 
# #                 afd_idx <- which(names(afd_list) %in% seq(start_year, last_year))
# #                 
# #                 if(length(afd_idx) == 1) {
# #                   afd <- afd_list[[afd_idx]]
# #                 }
# #                 
# #                 if(length(afd_idx) > 1) {
# #                   afd <-
# #                     afd_list[afd_idx] %>% 
# #                     data.table::rbindlist(fill = TRUE)
# #                 }
# #                 
# #                 fired_afd_temporal_match <- afd[(acq_year >= start_year) & (acq_year <= last_year)
# #                                                 & (acq_month >= start_month) & (acq_month <= last_month)
# #                                                 & (acq_day >= start_day) & (acq_day <= last_day)]
# #                 
# #                 if(nrow(fired_afd_temporal_match) > 0) {
# #                   this_fired_sf <- st_as_sf(tibble(id, start_year, start_month, start_day, last_year, last_month, last_day, geom = st_geometry(geom)), crs = fired_crs)
# #                   
# #                   fired_afd_spatiotemporal_match <-
# #                     fired_afd_temporal_match %>%
# #                     sf::st_as_sf() %>%
# #                     sf::st_intersection(this_fired_sf) %>%
# #                     sf::st_drop_geometry() %>%
# #                     data.table::as.data.table()
# #                   
# #                   if(nrow(fired_afd_spatiotemporal_match) == 0) {
# #                     fired_afd_spatiotemporal_match <- NULL
# #                   }
# #                   
# #                 } else {fired_afd_spatiotemporal_match <- NULL}
# #                 
# #                 rm(afd)
# #                 
# #                 return(fired_afd_spatiotemporal_match)
# #               })
# # 
# # fired_matches <- 
# #   fired_afd_spatiotemporal_match_list %>% 
# #   purrr::compact() %>% 
# #   rbindlist()
# 
# (Sys.time() - start)

