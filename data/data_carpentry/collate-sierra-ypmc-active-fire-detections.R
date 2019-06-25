# Purpose: create a single shapefile representing all the active fire detections in Sierra yellow pine/mixed-conifer

library(sf)
library(tidyverse)
library(purrr)
library(sp)

# function to create square buffer around fire points ---------------------
st_square_buffer <- function(obj, radius = NULL) {
  pts <- st_coordinates(obj)
  
  if(!is.null(radius)) {  
    xmin <- pts[, "X"] - radius
    xmax <- pts[, "X"] + radius
    ymin <- pts[, "Y"] - radius
    ymax <- pts[, "Y"] + radius
  } else {
    xmin <- pts[, "X"] - (pull(obj, SCAN) * 1000 / 2)
    xmax <- pts[, "X"] + (pull(obj, SCAN) * 1000 / 2)
    ymin <- pts[, "Y"] - (pull(obj, TRACK) * 1000 / 2)
    ymax <- pts[, "Y"] + (pull(obj, TRACK) * 1000 / 2)
  }
  
  corners <- tibble(xmin, xmax, ymin, ymax)
  
  square_polys <- 
    corners %>% 
    pmap(.f = function(xmin, xmax, ymin, ymax) {
      square_poly <- st_polygon(list(matrix(c(xmin, ymax, 
                                              xmax, ymax, 
                                              xmax, ymin, 
                                              xmin, ymin, 
                                              xmin, ymax), 
                                            byrow = TRUE, 
                                            ncol = 2)))
      return(square_poly)
    })
  
  new_obj <-
    obj %>%
    st_drop_geometry() %>% 
    dplyr::mutate(geometry = st_sfc(square_polys, crs = st_crs(obj))) %>% 
    st_as_sf()
  
  return(new_obj) 
}

# create single object of  active fire points ------------------------------

years <- 2000:2019

filenames <- paste0("data/data_raw/jepson_with_firepoints/jepson_sierra-nevada-ecoregion_", years, "_pts_conf50.shp")

# Dropped the "TYPE" column, which only appears in the 2019 data
firepoints <- 
  filenames %>% 
  map(.f = st_read) %>% 
  map2(.y = years, 
       .f = function(x, y) {
         x$year <- y
         x <- 
           x %>% 
           dplyr::select(LATITUDE, LONGITUDE, BRIGHTNESS, SCAN, TRACK, ACQ_DATE, ACQ_TIME, SATELLITE, INSTRUMENT, CONFIDENCE, VERSION, BRIGHT_T31, FRP, DAYNIGHT, year, geometry)
         return(x)
       }) %>% 
  do.call("rbind", .) %>% 
  dplyr::mutate(pt_id = 1:nrow(.))

firepoints3310 <-
  firepoints %>% 
  st_transform(3310) %>% 
  st_square_buffer()

firepoints

st_write(firepoints, "data/data_output/sierra-ypmc-active-fire-detections_epsg4326/sierra-ypmc-active-fire-detections_epsg4326.shp")
st_write(firepoints3310, "data/data_output/sierra-ypmc-active-fire-detections_epsg3310.gpkg")
