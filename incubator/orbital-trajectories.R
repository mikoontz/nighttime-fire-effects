# Purpose: Some various attempts at determining orbital trajectories for Aqua and Terra

library(tidyverse)
library(reticulate)
library(lubridate)
library(sf)
library(raster)
library(fasterize)
library(rnaturalearth)
library(viridis)

# function that creates bowtie polygon ------------------------------------
obj = orbit_sf[1, ]
modis_bowtie_buffer <- function(obj) {
  coords <- st_coordinates(obj)
  x <- coords[, 1]
  y <- coords[, 2]
  
  # 1.477 seconds per scan (Wolfe et al., 2002)
  # 10 km along-track distance in a single scan
  # 2340 km swath width
  # 1 / 1.477 [sec per scan] * 60 [sec per minute] * 10 [km along-track per scan] * 1000 [m per km]
  # Equals 406.22884 km along-track per minute
  
  dist_along_track_per_minute_m <- 1 / 1.477 * 60 * 10 * 1000
  swath_width_m <- 2340 * 1000
  
  # full-width offset
  x_offset <- swath_width_m / 2
  # nadir offset
  x_offset <- ((swath_width_m / 2) / (tan(55 * pi / 180)) * tan(24 * pi / 180))
  
  y_offset_small <- dist_along_track_per_minute_m / 2
  # Very slight bowtie flaring because only one additional scan's bowtie
  # effect gets added to the cumulative track-length from one minute of the
  # satellite's movement
  y_offset_large <- (dist_along_track_per_minute_m / 2) + 10000
  y_offset_large <- (dist_along_track_per_minute_m / 2)
  
  bowties <-
    coords %>%
    as_tibble() %>% 
    dplyr::rename_all(tolower) %>% 
    purrr::pmap(.f = function(x, y) {
      bowtie <- st_polygon(list(matrix(c(x, y + y_offset_small,
                                         x + x_offset, y + y_offset_large,
                                         x + x_offset,  y - y_offset_large,
                                         x, y - y_offset_small,
                                         x - x_offset, y - y_offset_large,
                                         x - x_offset, y + y_offset_large,
                                         x, y + y_offset_small),
                                       byrow = TRUE,
                                       ncol = 2)))
      return(bowtie)
    })
  
  new_obj <-
    obj %>%
    st_drop_geometry() %>% 
    dplyr::mutate(geometry = st_sfc(bowties, crs = st_crs(obj))) %>% 
    st_as_sf()
  
  return(new_obj)
}

plot(st_geometry(modis_bowtie_buffer(obj)))

# setup python pieces -----------------------------------------------------
# create a conda environment called "r-reticulate" if there isn't one already
# Include the pyorbital package in the install
# reticulate::conda_install("r-reticulate", packages = "pyorbital")
# Activate the "r-reticulate" environment
reticulate::use_condaenv("anaconda3/r-reticulate")

# datetime <- reticulate::import("datetime")
orb <- reticulate::import("pyorbital")

# get TLE files -----------------------------------------------------------
# The TLE comes in a single big text file, but only the single TLE for a particular
# time should be used for the orbital positioning. This requires breaking
# apart the giant table of TLEs into individual TLEs based on the date. This will
# also allow matching of the TLE dates to the nearest datetime when the orbit
# prediction is to be made. That is, if we want a predicted location of the
# Aqua satellite on 2017-04-12 at 0900, we want to use the TLE with a datetime
# closest to that day and time.

aqua_tle <- 
  read_fwf(file = "data/data_raw/aqua_27424_TLE_2002-06-01_2019-10-22.txt", fwf_widths(69)) %>% 
  dplyr::rename(line = X1) %>% 
  dplyr::mutate(line_number = as.numeric(substr(line, start = 1, stop = 1))) %>% 
  dplyr::mutate(id = rep(1:(n() / 2), each = 2),
                satellite = "aqua")

terra_tle <- 
  read_fwf(file = "data/data_raw/terra_25994_TLE_2002-06-01_2019-10-22.txt", fwf_widths(69)) %>% 
  dplyr::rename(line = X1) %>% 
  dplyr::mutate(line_number = as.numeric(substr(line, start = 1, stop = 1))) %>% 
  dplyr::mutate(id = rep(1:(n() / 2), each = 2),
                satellite = "terra")

tle <-
  rbind(aqua_tle, terra_tle)

# Info describing meaning of each character
# https://www.celestrak.com/NORAD/documentation/tle-fmt.php

tle_compact <-
  tle %>% 
  tidyr::pivot_wider(names_from = line_number, values_from = line) %>% 
  dplyr::rename(L1 = `1`, L2 = `2`) %>% 
  dplyr::mutate(yearstring = paste0("20", substr(L1, start = 19, stop = 20)),
                daystring = substr(L1, start = 21, stop = 32)) %>% 
  tidyr::separate(col = daystring, into = c("doy", "partial_day"), sep = "\\.") %>% 
  dplyr::mutate(partial_day = as.numeric(paste0("0.", partial_day)),
                doy = as.numeric(doy),
                hour_dec = 24 * partial_day,
                hour_int = floor(hour_dec),
                minute = round((hour_dec - hour_int) * 60),
                date = lubridate::ymd(paste0(yearstring, "-01-01"), tz = "zulu") + days(doy - 1) + hours(hour_int) + minutes(minute))



# simple orbital positions ------------------------------------------------

# These will grab the TLE files for the day they are run.
# First run on 2019-10-23 and TLE file saved in "data/data_raw/2019-10-23_TLE-files-for-satellite-orbit-predictions.txt"
aqua <- orb$orbital$Orbital("EOS-Aqua")
terra <- orb$orbital$Orbital("EOS-Terra")

todaytime <- today() %>% with_tz("zulu") %>% as.POSIXlt()
n_periods <- 1

(start <- Sys.time())
orbit_positions_simple <-
  tibble(datetime = seq(from = todaytime - days(n_periods * 16) - minutes(1), to = todaytime + days(n_periods * 16) - minutes(1), by = "1 min")) %>%
  dplyr::mutate(aqua = mapply(FUN = function(x) {list(aqua$get_lonlatalt(x))}, .$datetime),
                terra = mapply(FUN = function(x) {list(terra$get_lonlatalt(x))}, .$datetime)) %>%
  pivot_longer(cols = c(aqua, terra), names_to = "satellite", values_to = "location") %>%
  tidyr::hoist(.col = location, lon = 1, lat = 2, alt = 3)

(Sys.time() - start)

# orbital positions with ascending descending -----------------------------

(start <- Sys.time())
orbit_positions_asc <-
  tibble(datetime = seq(from = todaytime - days(n_periods * 16) - minutes(1), to = todaytime + days(n_periods * 16) - minutes(1), by = "1 min")) %>%
  dplyr::mutate(aqua = mapply(FUN = function(x) {list(aqua$get_lonlatalt(x))}, .$datetime),
                terra = mapply(FUN = function(x) {list(terra$get_lonlatalt(x))}, .$datetime)) %>%
  pivot_longer(cols = c(aqua, terra), names_to = "satellite", values_to = "location") %>%
  tidyr::hoist(.col = location, lon = 1, lat = 2, alt = 3) %>%
  dplyr::mutate(asc = ifelse(satellite == "aqua",
                             yes = mapply(FUN = function(x) {
                               ifelse(aqua$get_position(x, normalize = FALSE)[[2]][3, ] > 0,
                                      yes = "ascending_node",
                                      no = "descending_node")
                             }, .$datetime),
                             no = mapply(FUN = function(x) {
                               ifelse(terra$get_position(x, normalize = FALSE)[[2]][3, ] > 0,
                                      yes = "ascending_node",
                                      no = "descending_node")
                             }, .$datetime)))

(Sys.time() - start)


# using ascending/descending to visualize ---------------------------------

assign_segments <- function(switch, flag) {
  rep_times <- (c(1, which(switch), (length(switch) + 1)) - lag(c(1, which(switch), length(switch))))[-1]
  
  rep(paste0(flag, 1:length(rep_times)), times = rep_times)
}

sat_positions <-
  orbit_positions_asc %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE) %>% 
  st_transform("+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs") %>% 
  st_%>%
  group_by(satellite) %>%
  mutate(switch = asc != lag(asc)) %>%
  slice(-(1:2)) %>%
  dplyr::mutate(segment = ifelse(satellite == "aqua",
                                 yes = assign_segments(switch, "a"),
                                 no = assign_segments(switch, "t"))) %>% 
  ungroup()

sat_paths <-
  sat_positions %>%
  group_by(segment) %>%
  summarize(do_union = FALSE) %>%
  st_cast("LINESTRING") %>%
  st_buffer(dist = 2330000 / 2, endCapStyle = "FLAT") %>%
  st_cast("MULTIPOLYGON")

