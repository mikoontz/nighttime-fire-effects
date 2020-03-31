# Defining nighttime fire regime by FRP and detections

library(tidyverse)
library(raster)
library(data.table)
library(viridis)


# gridded MCD14ML data ---------------------------------------------------------

if(length(list.files("data/data_output/CSV_nocorn_grid_0_25_degree_vars/AFC_num/")) == 0) {
  download.file(url = "http://earthlab-jmcglinchy.s3-us-west-2.amazonaws.com/night_fire/gridded/vars_CSV_nocorn.zip", 
                destfile = "data/data_output/vars_CSV_nocorn.zip", 
                method = "curl")
  
  unzip(zipfile = "data/data_output/vars_CSV_nocorn.zip", exdir = "data/data_output/")
}


# fire detection count ----------------------------------------------------

afd_files <- list.files("data/data_output/CSV_nocorn_grid_0_25_degree_vars/AFC_num/") %>% 
  tibble::enframe(name = NULL) %>% 
  setNames("filename") %>% 
  tidyr::separate(col = filename, into = c("satellite", "daynight", "afc", "datatype", "month", "year", "fileextension"), remove = FALSE) %>% 
  dplyr::select(-satellite, -afc, -datatype, -fileextension) %>% 
  dplyr::mutate(year = as.numeric(year),
                daynight = ifelse(daynight == "D", yes = "day", no = "night")) %>% 
  dplyr::filter(year >= 2003)

read_and_stack_rasters <-  function(filename, daynight, month, year) {
  r <- raster::raster(paste0("data/data_output/CSV_nocorn_grid_0_25_degree_vars/AFC_num/", filename))
  return(r)
}

# Stack all 192 year-month rasters representing total detections
# and sum them
afd_day <-
  afd_files %>% 
  dplyr::filter(daynight == "day") %>% 
  purrr::pmap(.f = read_and_stack_rasters) %>% 
  do.call("stack", .) %>% 
  raster::calc(fun = sum)

# Repeat for night detections rasters
afd_night <-
  afd_files %>% 
  dplyr::filter(daynight == "night") %>% 
  purrr::pmap(.f = read_and_stack_rasters) %>% 
  do.call("stack", .) %>% 
  raster::calc(fun = sum)

raster::writeRaster(x = afd_day, filename = "data/data_output/total-detections_day_0.25.tif")
raster::writeRaster(x = afd_night, filename = "data/data_output/total-detections_night_0.25.tif")

afd_day <- raster::shift(x = afd_day, dx = -0.125, dy = 0.125)
afd_night <- raster::shift(x = afd_night, dx = -0.125, dy = 0.125)

# FRP data ----------------------------------------------------------------

frp_files <- list.files("data/data_output/CSV_nocorn_grid_0_25_degree_vars/FRP_total/") %>% 
  tibble::enframe(name = NULL) %>% 
  setNames("filename") %>% 
  tidyr::separate(col = filename, into = c("satellite", "daynight", "afc", "datatype", "month", "year", "fileextension"), remove = FALSE) %>% 
  dplyr::select(-satellite, -afc, -datatype, -fileextension) %>% 
  dplyr::mutate(year = as.numeric(year),
                daynight = ifelse(daynight == "D", yes = "day", no = "night")) %>% 
  dplyr::filter(year >= 2003)

read_and_stack_rasters <-  function(filename, daynight, month, year) {
  r <- raster::raster(paste0("data/data_output/CSV_nocorn_grid_0_25_degree_vars/FRP_total/", filename))
  return(r)
}

# Stack all 192 year-month rasters representing total FRP
# and then sum them
frp_day <-
  frp_files %>% 
  dplyr::filter(daynight == "day") %>% 
  purrr::pmap(.f = read_and_stack_rasters) %>% 
  do.call("stack", .) %>% 
  raster::calc(fun = sum)

# Repeat for the night rasters
frp_night <-
  frp_files %>% 
  dplyr::filter(daynight == "night") %>% 
  purrr::pmap(.f = read_and_stack_rasters) %>% 
  do.call("stack", .) %>% 
  raster::calc(fun = sum)

raster::writeRaster(x = afd_day, filename = "data/data_output/frp_day_0.25.tif", overwrite = TRUE)
raster::writeRaster(x = frp_night, filename = "data/data_output/frp_night_0.25.tif", overwrite = TRUE)

frp_day <- raster::shift(x = frp_day, dx = -0.125, dy = 0.125)
frp_night <- raster::shift(x = frp_night, dx = -0.125, dy = 0.125)

# overpass count ----------------------------------------------------------

if(!file.exists("data/data_output/2003-2018_day_overpass-count.tif")) {
  download.file(url = "https://earthlab-mkoontz.s3-us-west-2.amazonaws.com/MODIS-overpass-counts_0.25_analysis-ready/2003-2018/2003-2018_day_overpass-count.tif",
                destfile = "data/data_output/2003-2018_day_overpass-count.tif",
                method = "curl")
}

if(!file.exists("data/data_output/2003-2018_night_overpass-count.tif")) {
  download.file(url = "https://earthlab-mkoontz.s3-us-west-2.amazonaws.com/MODIS-overpass-counts_0.25_analysis-ready/2003-2018/2003-2018_night_overpass-count.tif",
                destfile = "data/data_output/2003-2018_night_overpass-count.tif",
                method = "curl")
}

# Represents count of all day (or night) overpasses between 2003 and 2018
day_overpass_count <- raster::raster("data/data_output/2003-2018_day_overpass-count.tif")
night_overpass_count <- raster::raster("data/data_output/2003-2018_night_overpass-count.tif")

day_overpass_count <- raster::shift(day_overpass_count, dx = -0.125, dy = 0.125)
night_overpass_count <- raster::shift(night_overpass_count, dx = -0.125, dy = 0.125)

# landcover ---------------------------------------------------------------

# Just use the 11 landcovers that we used to derive our VPDt
landcovers_of_interest <- c(1:2, 4:10, 12, 14)

landcover <- raster::raster(x = "data/data_raw/GLDASp4_domveg_025d.nc4")
# In the 4326 coordinate reference system, pixels have different
# areas, so we need to account for that as we do aggregations of the
# gridded product
landcover_area <- raster::area(landcover)
s <- stack(landcover, landcover_area)

# The lookup table to convert landcover index (in the raster) to
# the landcover name
landcover_table <- 
  readr::read_csv(file = "data/data_raw/GLDASp4_domveg_025d_lookup-table.csv")

# get all the values of the landcover raster including the lon/lat
# coordinates so we can join it with the 0.25 grid fire detections and
# frp gridded data
landcover_df <- 
  s %>% 
  as.data.frame(xy = TRUE) %>% 
  setNames(c("lon", "lat", "index", "pixel_area_km2")) %>% 
  dplyr::left_join(landcover_table, by = "index")

landcover_DT <- as.data.table(landcover_df)
data.table::setkeyv(landcover_DT, cols = c("lon", "lat"))

total_landcover_areas <-
  landcover_DT %>% 
  dplyr::group_by(index) %>% 
  dplyr::summarize(total_area_of_landcover_km2 = as.numeric(sum(pixel_area_km2, na.rm = TRUE))) %>% 
  dplyr::left_join(landcover_table, by = "index")


# correct for overpasses --------------------------------------------------
day_DT <-
  raster::stack(afd_day, frp_day, day_overpass_count) %>% 
  as.data.frame(xy = TRUE) %>% 
  setNames(c("lon", "lat", "total_detections", "frp", "overpasses")) %>% 
  dplyr::mutate(daynight = "day") %>% 
  dplyr::filter(lat >= -90) %>% 
  as.data.table()

night_DT <-
  raster::stack(afd_night, frp_night, night_overpass_count) %>% 
  as.data.frame(xy = TRUE) %>% 
  setNames(c("lon", "lat", "total_detections", "frp", "overpasses")) %>% 
  dplyr::mutate(daynight = "night") %>% 
  dplyr::filter(lat >= -90) %>% 
  as.data.table()

daynight_DT <- rbind(day_DT, night_DT)

daynight_DT <- daynight_DT[, `:=`(detections_per_overpass = total_detections / overpasses,
                                  frp_per_overpass = frp / overpasses)]

daynight_DT <- daynight_DT[!is.na(detections_per_overpass)]
data.table::setkey(x = daynight_DT, lon, lat, daynight)

# join fire product with landcover -----------------------------------------------------

lon_lat_landcover <- landcover_DT[daynight_DT]

# lon_lat_landcover[, `:=`(detections_per_overpass_per_km2 = total_detections / overpasses / pixel_area_km2,
#                          frp_per_overpass_per_km2 = frp / overpasses / pixel_area_km2)]


# summarize across all pixels belonging to each landcover type (and daynight)
afd_frp_landcover <- lon_lat_landcover[index %in% landcovers_of_interest, .(area_km2 = sum(pixel_area_km2),
                                                                            detections_per_overpass = sum(detections_per_overpass),
                                                                            frp_per_overpass = sum(frp_per_overpass)),
                                       by = .(index, daynight)]

afd_frp_landcover <- afd_frp_landcover[, `:=`(detections_per_overpass_per_1e6km2 = 1e6 * detections_per_overpass / area_km2,
                                              frp_per_overpass_per_1e6km2 = 1e6 * frp_per_overpass / area_km2)]

# pivot wider to put similar day and night measures in separate columns (so we can get
# percent)
afd_frp_landcover_wide <-
  afd_frp_landcover %>% 
  dplyr::select(index, daynight, detections_per_overpass_per_1e6km2, frp_per_overpass_per_1e6km2) %>% 
  tidyr::pivot_wider(names_from = "daynight", values_from = c("detections_per_overpass_per_1e6km2", "frp_per_overpass_per_1e6km2")) %>% 
  dplyr::mutate(pct_night_afd = 100 * detections_per_overpass_per_1e6km2_night / (detections_per_overpass_per_1e6km2_night + detections_per_overpass_per_1e6km2_day),
                pct_night_frp = 100 * frp_per_overpass_per_1e6km2_night / (frp_per_overpass_per_1e6km2_night + frp_per_overpass_per_1e6km2_day))

# Make table suitable for printing
night_fire_regime_table <-
  afd_frp_landcover_wide %>% 
  dplyr::left_join(landcover_table) %>% 
  dplyr::filter(index %in% landcovers_of_interest) %>% 
  dplyr::mutate(landcover = factor(landcover, levels = c("Evergreen Needleleaf Forest, Evergreen Broadleaf Forest, Deciduous Broadleaf Forest, Mixed Forest, Closed Shrublands, Open Shrublands, Woody Savannas, Savannas, Grassland, Cropland, Cropland/Natural Vegetation Mosaic"))) %>% 
  dplyr::arrange(landcover) %>% 
  dplyr::select(-index) %>% 
  dplyr::select(landcover, 
                detections_per_overpass_per_1e6km2_day, 
                detections_per_overpass_per_1e6km2_night,
                pct_night_afd,
                frp_per_overpass_per_1e6km2_day,
                frp_per_overpass_per_1e6km2_night, 
                pct_night_frp) %>% 
  dplyr::mutate_if(is.numeric, round, 2)

night_fire_regime_table

readr::write_csv(x = night_fire_regime_table, path = "tables/night-fire-regime-table.csv")
