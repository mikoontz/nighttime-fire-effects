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
library(geosphere)
library(mgcv)
library(purrr)
library(furrr)
library(devtools)
devtools::install_github('cloudyr/aws.s3', ref = '46aa3db34b89946205e6f27ce6f745cea4bf4571')
library(aws.s3)

# function that creates bowtie polygon ------------------------------------

# modis_bowtie_buffer <- function(obj, nadir = FALSE, bowtie = FALSE) {
#   
#   # 1.477 seconds per scan (Wolfe et al., 2002)
#   # 10 km along-track distance in a single scan
#   # 2340 km swath width
#   # 1 / 1.477 [sec per scan] * 60 [sec per minute] * 10 [km along-track per scan] * 1000 [m per km]
#   # Equals 406.22884 km along-track per minute
#   
#   dist_along_track_per_minute_m <- 1 / 1.477 * 60 * 10 * 1000
#   swath_width_m <- 2330 * 1000
#   
#   # orbital inclination of 98.2 degrees (i.e., 8.2 degrees west of due north on ascending node)
#   # not ready to say that this is a definitely usable piece of this footprint creation, so
#   # keeping it to 0 for now (to make footprints always be oriented north/south). Perhaps
#   # better to use the swath width along the predicted orbit path, but that has its own challenges
#   orbit_incl_offset <- 0
#   
#   # full-width offset or nadir offset?
#   # assume a much narrower swath width if just considering the pixels closest to nadir
#   # here, I chose a 24-degree scan angle as still being nadir because this is
#   # the widest angle at which there is no overlap with other scans
#   
#   x_offset <- ifelse(nadir, 
#                      yes = ((swath_width_m / 2) / (tan(55 * pi / 180)) * tan(24 * pi / 180)),
#                      no = swath_width_m / 2)
#   
#   y_offset_small <- dist_along_track_per_minute_m / 2
#   
#   # Very slight bowtie flaring because only one additional scan's bowtie
#   # effect gets added to the cumulative track-length from one minute of the
#   # satellite's movement
#   # Option to turn off this flaring in the 'bowtie=' argument; default is to square off the 
#   # footprint, rather than to bowtie.
#   y_offset_large <- ifelse(nadir,
#                            yes = y_offset_small,
#                            no = ifelse(bowtie, 
#                                        yes = (dist_along_track_per_minute_m / 2) + 10000,
#                                        no = y_offset_small))
#   
#   # The bowtie is defined by 6 points relative to the footprint center. 
#   # Even if the bowtie is squared off and is just a rectangle,
#   # we still define 6 points.
#   # pt1 is straight forward from the lon/lat in the along-track direction
#   # pt2 is forward in the along track direction and right in the along scan direction
#   # pt3 is behind in the along track direction and right in the along scan direction
#   # pt4 is straight behind in the along track direction
#   # pt5 is behind in the along track direction and left in the along scan direction
#   # pt6 is forward in the along track direction and left in the along scan direction
#   
#   hypotenuse_dist <- sqrt(y_offset_large ^ 2 + x_offset ^ 2)
#   corner_angles <- atan(y_offset_large / x_offset) * 180 / pi
#   
#   bowties <-
#     obj %>% 
#     st_drop_geometry() %>% 
#     dplyr::select(lon, lat) %>% 
#     purrr::pmap(.f = function(lon, lat) {
#       
#       pt1 <- geosphere::destPoint(p = c(lon, lat), b = 0 - orbit_incl_offset, d = y_offset_small)
#       pt2 <- geosphere::destPoint(p = c(lon, lat), b = 90 - orbit_incl_offset - corner_angles, d = hypotenuse_dist)
#       pt3 <- geosphere::destPoint(p = c(lon, lat), b = 90 - orbit_incl_offset + corner_angles, d = hypotenuse_dist)
#       pt4 <- geosphere::destPoint(p = c(lon, lat), b = 180 - orbit_incl_offset, d = y_offset_small)
#       pt5 <- geosphere::destPoint(p = c(lon, lat), b = 270 - orbit_incl_offset - corner_angles, d = hypotenuse_dist)
#       pt6 <- geosphere::destPoint(p = c(lon, lat), b = 270 - orbit_incl_offset + corner_angles, d = hypotenuse_dist)
#       
#       n_pts <- 3
#       
#       bowtie <-
#         rbind(
#           pt1,
#           geosphere::gcIntermediate(pt1, pt2, n = n_pts),
#           pt2,
#           geosphere::gcIntermediate(pt2, pt3, n = n_pts),
#           pt3,
#           geosphere::gcIntermediate(pt3, pt4, n = n_pts),
#           pt4,
#           geosphere::gcIntermediate(pt4, pt5, n = n_pts),
#           pt5,
#           geosphere::gcIntermediate(pt5, pt6, n = n_pts),
#           pt6,
#           geosphere::gcIntermediate(pt6, pt1, n = n_pts),
#           pt1) %>% 
#         list() %>% 
#         st_polygon()
#       
#       return(bowtie)
#     })
#   
#   new_obj <-
#     obj %>%
#     st_drop_geometry() %>% 
#     dplyr::mutate(geometry = st_sfc(bowties, crs = st_crs(obj))) %>% 
#     st_as_sf() %>% 
#     st_wrap_dateline()
#   
#   return(new_obj)
# }


# function creates the footprint of each minute's MODIS imagery -----------

modis_footprint_buffer <- function(obj, nadir = FALSE, bowtie = FALSE) {
  
  swath_width_m <- 2330 * 1000
  
  x_offset <- swath_width_m / 2

  footprints <-
    obj %>% 
    st_drop_geometry() %>% 
    dplyr::select(this_lon, this_lat, next_lon, next_lat) %>% 
    purrr::pmap(.f = function(this_lon, this_lat, next_lon, next_lat) {
      
      # 
      #       pt3-------pt4-------pt5
      #       |                     |
      #       |                     |
      #       |                     |
      #       pt2-------pt1-------pt6
      
      pt1 <- c(this_lon, this_lat)
      pt4 <- c(next_lon, next_lat)
      
      # initial and final bearing in degrees
      init_bearing <- geosphere::bearing(p1 = pt1, p2 = pt4)
      final_bearing <- geosphere::finalBearing(p1 = pt1, p2 = pt4)
      
      fudge_factor <- 0
      # fudge_factor <- 4
      pt2 <- geosphere::destPoint(p = c(this_lon, this_lat), b = init_bearing - 90 + fudge_factor, d = x_offset)
      pt3 <- geosphere::destPoint(p = c(next_lon, next_lat), b = final_bearing - 90 + fudge_factor, d = x_offset)
      
      pt5 <- geosphere::destPoint(p = c(next_lon, next_lat), b = final_bearing + 90 + fudge_factor, d = x_offset)
      pt6 <- geosphere::destPoint(p = c(this_lon, this_lat), b = init_bearing + 90 + fudge_factor, d = x_offset)
      
      plot(st_buffer(st_linestring(rbind(pt1, pt4)), dist = x_offset, endCapStyle = "FLAT"), axes = TRUE)
      plot(st_linestring(rbind(pt1, pt4)), axes = TRUE, add = TRUE)
      
      # poly <- st_buffer(st_linestring(rbind(pt1, pt4)), dist = x_offset, endCapStyle = "FLAT")
      n_pts <- 3
      
      poly <-
        rbind(
          pt1,
          geosphere::gcIntermediate(pt1, pt2, n = n_pts),
          pt2,
          geosphere::gcIntermediate(pt2, pt3, n = n_pts),
          pt3,
          geosphere::gcIntermediate(pt3, pt4, n = n_pts),
          pt4,
          geosphere::gcIntermediate(pt4, pt5, n = n_pts),
          pt5,
          geosphere::gcIntermediate(pt5, pt6, n = n_pts),
          pt6,
          geosphere::gcIntermediate(pt6, pt1, n = n_pts),
          pt1) %>% 
        list() %>% 
        st_polygon()
      return(poly)
      
    })
  
  new_obj <-
    obj %>%
    st_drop_geometry() %>% 
    dplyr::mutate(geometry = st_sfc(footprints, crs = st_crs(obj))) %>% 
    st_as_sf() %>% 
    st_wrap_dateline()
  
  return(new_obj)
}


# setup python pieces -----------------------------------------------------
# create a conda environment called "r-reticulate" if there isn't one already
# Include the pyorbital package in the install
reticulate::conda_install("r-reticulate", packages = "pyorbital", pip = TRUE, forge = TRUE, python_version = "3.7")
# conda_remove("r-reticulate")

# Activate the "r-reticulate" environment
reticulate::use_condaenv("r-reticulate")

if(Sys.info()['sysname'] == "Windows") {
  orb <- reticulate::import("pyorbital.orbital")
} else {
  orb <- reticulate::import("pyorbital")
  orb <- orb$orbital}

# get TLE files -----------------------------------------------------------
# The TLE comes in a single big text file, but only the single TLE for a particular
# time should be used for the orbital positioning. This requires breaking
# apart the giant table of TLEs into individual TLEs based on the date. This will
# also allow matching of the TLE dates to the nearest datetime when the orbit
# prediction is to be made. That is, if we want a predicted location of the
# Aqua satellite on 2017-04-12 at 0900, we want to use the TLE with a datetime
# closest to that day and time.

if(!dir.exists("data/data_output")) {
  dir.create("data/data_output", recursive = TRUE)
}

if(!file.exists("data/data_output/aqua_27424_TLE_2002-05-04_2019-12-31.txt")) {
  
  aws.s3::save_object(object = "aqua-terra-overpass-corrections/aqua_27424_TLE_2002-05-04_2019-12-31.txt", 
                      bucket = "earthlab-mkoontz",
                      file = "data/data_output/aqua_27424_TLE_2002-05-04_2019-12-31.txt")
}

aqua_tle <-
  read_fwf(file = "data/data_raw/aqua_27424_TLE_2002-05-04_2019-12-31.txt", fwf_widths(69)) %>% 
  dplyr::rename(line = X1) %>% 
  dplyr::mutate(line_number = as.numeric(substr(line, start = 1, stop = 1))) %>% 
  dplyr::mutate(id = rep(1:(n() / 2), each = 2),
                satellite = "Aqua")

if(!file.exists("data/data_output/terra_25994_TLE_1999-12-18_2019-12-31.txt")) {
  
  aws.s3::save_object(object = "aqua-terra-overpass-corrections/terra_25994_TLE_1999-12-18_2019-12-31.txt", 
                      bucket = "earthlab-mkoontz", 
                      file = "data/data_output/terra_25994_TLE_1999-12-18_2019-12-31.txt")
  
} 

terra_tle <- 
  read_fwf(file = "data/data_output/terra_25994_TLE_1999-12-18_2019-12-31.txt", fwf_widths(69)) %>% 
  dplyr::rename(line = X1) %>% 
  dplyr::mutate(line_number = as.numeric(substr(line, start = 1, stop = 1))) %>% 
  dplyr::mutate(id = rep(1:(n() / 2), each = 2),
                satellite = "Terra")

tle <-
  rbind(aqua_tle, terra_tle)

# Info describing meaning of each character
# https://www.celestrak.com/NORAD/documentation/tle-fmt.php
# assign some attributes to each TLE so they can be properly subset and matched
# to the desired time of orbit prediction
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

start_date <- ymd("2019-01-01", tz = "zulu")
n_periods <- 1/16
end_date <- start_date + days(n_periods * 16)
# end_date <- start_date + minutes(2)

# 22.5 minutes to run on Macbook Pro for 3 periods (3*16 days) 
# 11.75 minutes on the Alien

# dates_of_interest <-
#   expand.grid(datetime = seq(start_date - minutes(1), end_date - minutes(1), by = "1 min"), satellite = c("Aqua", "Terra")) %>%
#   as_tibble() %>%
#   dplyr::arrange(datetime) %>%
#   dplyr::mutate(satellite = as.character(satellite))


# First create a column in a data.frame representing the minute-ly sequence of datetimes
# starting from the start date and continuing for an integer number of periods
# The position of Aqua at that datetime is determined by first figuring out which of the
# Aqua TLE is closest in time to the time we want to predict for.
# Using this TLE, we calculate the longitude, latitude, and altitude using pyorbital
# We iterate through all datetimes using a for loop (in the foreach package, so we can parallelize)
# Then, we do the same thing to get the Terra longitude, latitude, and altitude at that
# datetime.
# Note that we need to include the final Python to R translation piece within each mapped function
# in order for this to work in parallel

# Combine Aqua and Terra operations by using an expanded initial dataframe
terra_dates_of_interest <-
  tibble(datetime = seq(ymd("1999-12-18", tz = "zulu") - minutes(1), ymd("2019-12-31", tz = "zulu") - minutes(1), by = "1 min"),
         satellite = "Terra")

aqua_dates_of_interest <-
  tibble(datetime = seq(ymd("2002-05-04", tz = "zulu") - minutes(1), ymd("2019-12-31", tz = "zulu") - minutes(1), by = "1 min"),
         satellite = "Aqua")

dates_of_interest <-
  rbind(terra_dates_of_interest, aqua_dates_of_interest) %>%
  dplyr::arrange(datetime)


(start <- Sys.time())
plan(multiprocess)

orbit_positions <-
  dates_of_interest %>%
  dplyr::mutate(location = furrr::future_map2(.x = datetime, .y = satellite, .progress = TRUE, .f = function(x, y) {
    
    # find the TLE for the correct satellite closest to the time at which we want to predict satellite position
    this_tle <-
      tle_compact %>%
      dplyr::filter(satellite == y) %>%
      dplyr::mutate(difftime_to_tle = as.numeric(date - x)) %>% 
      dplyr::mutate(prior_or_next_tle = ifelse(difftime_to_tle < 0, yes = "prior", no = "next"))
    
    closest_tle <-
      this_tle %>% 
      dplyr::filter(rank(abs(difftime_to_tle), ties.method = "first") == 1)
    
    prior_tle <-
      this_tle %>% 
      dplyr::filter(prior_or_next_tle == "prior") %>% 
      dplyr::filter(rank(abs(difftime_to_tle), ties.method = "first") == 1)
    
    next_tle <-
      this_tle %>% 
      dplyr::filter(prior_or_next_tle == "next") %>% 
      dplyr::filter(rank(abs(difftime_to_tle), ties.method = "first") == 1)
    
    if(Sys.info()['sysname'] == "Windows") {
      orb <- reticulate::import("pyorbital.orbital")
    } else {
      orb <- reticulate::import("pyorbital")
      orb <- orb$orbital}
    
    # create an instance of the Orbital class that will let us make satellite position
    # predictions
    this_orbital_info <-
      orb$Orbital(paste0("EOS-", y), line1 = closest_tle$L1, line2 = closest_tle$L2)
    
    # get the longitude, latitude, and altitude of the satellite at datetime 'x'
    this_lonlatalt <- this_orbital_info$get_lonlatalt(x)
    
    # is the satellite on its ascending or descending node (based on velocity in the 'z'
    # direction; if positive, satellite is ascending south to north. If negative, satellite
    # is descending north to south)
    # useful for turning multipoints into linestrings representing the satellite path, which
    # is not currently implemented. That code can be retrieved if we want to go that route
    # instead. Basically, we turn each ascending or descending pass for each satellite into
    # its own linestring, then buffer those linestrings with the swath width of the satellites,
    # then use those noodle-looking orbit paths as polygons to rasterize. Challenging to get
    # both the poles and the equator to work well using this method!
    this_asc <- ifelse(this_orbital_info$get_position(x, normalize = FALSE)[[2]][3, ] > 0,
                       yes = "ascending_node",
                       no = "descending_node")
    
    return(c(y, this_lonlatalt, this_asc, 
             prior_tle$date, prior_tle$difftime_to_tle, prior_tle$L1, prior_tle$L2,
             next_tle$date, next_tle$difftime_to_tle, next_tle$L1, next_tle$L2))
    
  })) %>%
  tidyr::hoist(.col = location, 
               satellite = 1, lon = 2, lat = 3, alt = 4, asc = 5, 
               prior_tle_datetime = 6, prior_tle_difftime = 7, prior_tle_L1 = 8, prior_tle_L2 = 9,
               next_tle_datetime = 10, next_tle_difftime = 11, next_tle_L1 = 12, next_tle_L2 = 13,
               .ptype = list(satellite = character(), lon = numeric(), lat = numeric(), alt = numeric(), asc = character(), 
                             prior_tle_datetime = vctrs::new_datetime(), 
                             prior_tle_difftime = numeric(), 
                             prior_tle_L1 = character(), prior_tle_L2 = character(),
                             next_tle_datetime = vctrs::new_datetime(), 
                             next_tle_difftime = numeric(), 
                             next_tle_L1 = character(), next_tle_L2 = character()))

plan(sequential)

data.table::fwrite(x = orbit_positions, file = "data/data_output/orbit-positions_aqua-terra.csv")

aws.s3::s3write_using(x = orbit_positions, 
                      FUN = data.table::fwrite, 
                      object = "aqua-terra-overpass-corrections/orbit-positions_aqua-terra.csv",
                      bucket = "earthlab-mkoontz")

(Sys.time() - start)




# make object spatial ---------------------------------------------------------------

modis_crs <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
orbit_sf <- 
  st_as_sf(orbit_positions, coords = c("lon", "lat"), crs = 4326, remove = FALSE) %>% 
  # st_transform(modis_crs) %>% 
  dplyr::mutate(this_lon = st_coordinates(.)[, 1],
                this_lat = st_coordinates(.)[, 2]) %>% 
  dplyr::arrange(satellite, datetime) %>% 
  dplyr::mutate(next_lon = lead(this_lon), 
                next_lat = lead(this_lat))

# build bowties around each orbit position ---------------------------------------------------------------
# 8 minutes to build polygons from points on the Macbook Pro
test_footprints <- modis_footprint_buffer(obj)

(start <- Sys.time())
sat_footprints <- 
  modis_bowtie_buffer(orbit_sf) %>% 
  st_wrap_dateline(options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180")) %>% 
  st_cast("MULTIPOLYGON") %>% 
  dplyr::mutate(year = year(datetime),
                month = month(datetime),
                day = day(datetime),
                hour = hour(datetime),
                minute = minute(datetime))
(Sys.time() - start)

sf::st_write(obj = sat_footprints, dsn = "data/data_output/aqua-terra-footprints.gpkg", delete_dsn = TRUE)

# rasterize the overlapping image footprints to a regular grid (using one of Joe's as
# a template)
r_0.25 <- raster::raster("data/data_raw/grid_0_25_degree_vars_modis_D_AFC_num_April_2001.tif")
r_2.5 <- raster::raster("data/data_raw/grid_2_5_degree_vars_modis_D_AFC_num_April_2001.tif")

orbit_overlap_0.25 <- 
  fasterize::fasterize(sf = sat_footprints, raster = r_0.25, fun = "count")
orbit_overlap_0.25 <- orbit_overlap_0.25 / (n_periods * 16)

orbit_overlap_2.5 <- 
  fasterize::fasterize(sf = sat_footprints, raster = r_2.5, fun = "count")
orbit_overlap_2.5 <- orbit_overlap_2.5 / (n_periods * 16)

# visualize
plot(orbit_overlap_0.25, col = viridis(30))
plot(st_as_sf(ne_coastline()) %>% st_geometry(), add = TRUE)

plot(orbit_overlap_2.5, col = viridis(30))
plot(st_as_sf(ne_coastline()) %>% st_geometry(), add = TRUE)

# write to disk
dir.create("analyses/analyses_output")
writeRaster(x = orbit_overlap_0.25, filename = "analyses/analyses_output/aqua-terra-overpasses-per-day_0.25-degree-grid.tif", overwrite = TRUE)
writeRaster(x = orbit_overlap_2.5, filename = "analyses/analyses_output/aqua-terra-overpasses-per-day_2.5-degree-grid.tif", overwrite = TRUE)

# Build a table demonstrating the empirical function that maps latitude to expected number of overpasses
# per day
samps_0.25 <-
  expand.grid(seq(-180, 180, by = 5), seq(-90, 90, by = 0.25)) %>% 
  setNames(c("lon", "lat")) %>% 
  as_tibble() %>% 
  dplyr::mutate(overpasses = extract(x = orbit_overlap_0.25, y = ., method = "bilinear")) %>% 
  dplyr::filter(!is.na(overpasses))

samps_2.5 <-
  expand.grid(seq(-180, 180, by = 5), seq(-90, 90, by = 2.5)) %>% 
  setNames(c("lon", "lat")) %>% 
  as_tibble() %>% 
  dplyr::mutate(overpasses = extract(x = orbit_overlap_2.5, y = ., method = "bilinear")) %>% 
  dplyr::filter(!is.na(overpasses))

# include the range of observed overpasses as a minimum and maximum attribute
overpass_corrections_0.25 <- 
  samps_0.25 %>%
  group_by(lat) %>% 
  summarize(mean_overpasses = mean(overpasses),
            min_overpasses = min(overpasses),
            max_overpasses = max(overpasses))

overpass_corrections_2.5 <- 
  samps_2.5 %>%
  group_by(lat) %>% 
  summarize(mean_overpasses = mean(overpasses),
            min_overpasses = min(overpasses),
            max_overpasses = max(overpasses))

# write to disk
write.csv(overpass_corrections_0.25, file = "analyses/analyses_output/aqua-terra-overpass-corrections-table_0.25-degree-grid.csv", row.names = FALSE)
write.csv(overpass_corrections_2.5, file = "analyses/analyses_output/aqua-terra-overpass-corrections-table_2.5-degree-grid.csv", row.names = FALSE)

# save the visualization to disk
png("figures/aqua-terra-overpass-corrections-map_0.25-degree-grid.png")
plot(orbit_overlap_0.25, col = viridis(30))
plot(st_as_sf(ne_coastline()) %>% st_geometry(), add = TRUE)
dev.off()

png("figures/aqua-terra-overpass-corrections-map_2.5-degree-grid.png")
plot(orbit_overlap_2.5, col = viridis(30))
plot(st_as_sf(ne_coastline()) %>% st_geometry(), add = TRUE)
dev.off()

# save the empirical model plot to disk
ggplot(overpass_corrections_0.25 %>% filter(lat %in% c(seq(-83.5, -70, by = 0.25), -69:69, seq(70, 83.5, by = 0.25))), aes(x = lat, y = mean_overpasses)) +
  geom_point(cex = 0.3) +
  theme_bw() +
  geom_ribbon(aes(ymin = min_overpasses, ymax = max_overpasses), fill = "red", alpha = 0.1)
ggsave("figures/aqua-terra-overpass-corrections-function_0.25-degree-grid.png")

ggplot(overpass_corrections_2.5 %>% filter(lat %in% c(seq(-83.5, -70, by = 0.25), -69:69, seq(70, 83.5, by = 0.25))), aes(x = lat, y = mean_overpasses)) +
  geom_point(cex = 0.3) +
  theme_bw() +
  geom_ribbon(aes(ymin = min_overpasses, ymax = max_overpasses), fill = "red", alpha = 0.1)
ggsave("figures/aqua-terra-overpass-corrections-function_2.5-degree-grid.png")
