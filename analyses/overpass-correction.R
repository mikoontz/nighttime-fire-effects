# Purpose: overpass correction for Aqua / Terra to account for more possible
# overpasses at higher latitudes.

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
modis_bowtie_buffer <- function(obj, nadir = FALSE) {
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

  # full-width offset or nadir offset?
  x_offset <- ifelse(nadir, 
                     yes = ((swath_width_m / 2) / (tan(55 * pi / 180)) * tan(24 * pi / 180)),
                     no = swath_width_m / 2)

  y_offset_small <- dist_along_track_per_minute_m / 2
  
  # Very slight bowtie flaring because only one additional scan's bowtie
  # effect gets added to the cumulative track-length from one minute of the
  # satellite's movement
  y_offset_large <- ifelse(nadir,
                           yes = y_offset_small,
                           no = (dist_along_track_per_minute_m / 2) + 10000)

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

plot(st_geometry(modis_bowtie_buffer(obj, nadir = TRUE)))
plot(st_geometry(modis_bowtie_buffer(obj, nadir = FALSE)))

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


# complex orbital positions -----------------------------------------------

start_date <- ymd("2018-01-01", tz = "zulu")
n_periods <- 5

(start <- Sys.time())
orbit_positions <-
  tibble(datetime = seq(start_date, start_date + days(n_periods * 16) - minutes(1), by = "1 min")) %>% 
  dplyr::mutate(aqua = mapply(FUN = function(x) {
    
    closest_aqua_tle <-
      tle_compact %>% 
      dplyr::filter(satellite == "aqua") %>% 
      dplyr::filter(rank(abs(as.numeric(date - x)), ties.method = "first") == 1)
    
    this_orbital_info <-
      orb$orbital$Orbital("EOS-Aqua", line1 = closest_aqua_tle$L1, line2 = closest_aqua_tle$L2)
    
    return(list(this_orbital_info$get_lonlatalt(x)))
    
  }, .$datetime),
  terra = mapply(FUN = function(x) {
    
    closest_terra_tle <-
      tle_compact %>%
      dplyr::filter(satellite == "terra") %>%
      dplyr::filter(rank(abs(as.numeric(date - x)), ties.method = "first") == 1)
    
    this_orbital_info <-
      orb$orbital$Orbital("EOS-Terra", line1 = closest_terra_tle$L1, line2 = closest_terra_tle$L2)
    
    return(list(this_orbital_info$get_lonlatalt(x)))
    
  }, .$datetime)) %>%
  pivot_longer(cols = c(aqua, terra), names_to = "satellite", values_to = "location") %>%
  tidyr::hoist(.col = location, lon = 1, lat = 2, alt = 3)

(Sys.time() - start)

# rasterize ---------------------------------------------------------------

coast_sinu <- 
  st_as_sf(ne_coastline()) %>% 
  st_transform("+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

orbit_sf <- 
  st_as_sf(orbit_positions, coords = c("lon", "lat"), crs = 4326, remove = FALSE) %>% 
  st_transform("+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

sat_footprints <- 
  modis_bowtie_buffer(orbit_sf)

r_sinu <- 
  raster::raster("data/data_raw/mcd14ml-rasterized-template_0.25-degrees.tif") %>% 
  raster::projectRaster(crs = "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

orbit_overlap_sinu <- 
  fasterize::fasterize(sf = sat_footprints, raster = r_sinu, fun = "count")

# Get average daily overlap
orbit_overlap_sinu <- orbit_overlap_sinu / (n_periods * 16)

plot(orbit_overlap_sinu, col = viridis(100))
plot(st_geometry(coast_sinu), add = TRUE)

orbit_overlap_4326 <- 
  orbit_overlap_sinu %>% 
  projectRaster(crs = st_crs(4326)$proj4string)

plot(orbit_overlap_4326, col = viridis(30))
