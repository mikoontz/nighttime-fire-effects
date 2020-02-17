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

api_keys <- read_csv("data/data_raw/LAADS-DAAC_api-keys.csv")

aqua_years <- 2002:2019
terra_years <- 2000:2019

if(!dir.exists(paths = file.path("data", "data_output", "MODIS-footprints", "TERRA"))) {
  
  dir.create(path = file.path("data", "data_output", "MODIS-footprints", "TERRA"))
  
  lapply(terra_years[-1], FUN = function(this_year) {
    system2(command = "wget", args = paste0('-e robots=off -m -np -R .html,.tmp -nH --cut-dirs=3 "https://ladsweb.modaps.eosdis.nasa.gov/archive/geoMeta/61/TERRA/', this_year, '/" --header "Authorization: Bearer ', dplyr::filter(api_keys, satellite == "Terra") %>% dplyr::pull(key), '" -P data/data_output/MODIS-footprints/'))
  })
  
}
if(!dir.exists(paths = file.path("data", "data_output", "MODIS-footprints", "AQUA"))) {
  
  dir.create(path = file.path("data", "data_output", "MODIS-footprints", "AQUA"))
  
  lapply(aqua_years, FUN = function(this_year) {
    system2(command = "wget", args = paste0('-e robots=off -m -np -R .html,.tmp -nH --cut-dirs=3 "https://ladsweb.modaps.eosdis.nasa.gov/archive/geoMeta/61/AQUA/', this_year, '/" --header "Authorization: Bearer ',  dplyr::filter(api_keys, satellite == "Aqua") %>% dplyr::pull(key), '" -P data/data_output/MODIS-footprints/'))
  })
  
}


aqua <- 
  read_delim("data/data_output/MODIS-footprints/AQUA/2002/MYD03_2002-07-04.txt", delim = ",", skip = 2) %>% 
  dplyr::rename(GranuleID = `# GranuleID`) %>% 
  # slice(8) %>% 
  # dplyr::select(-ends_with("BoundingCoord")) %>% 
  dplyr::mutate(geometry = pmap(.l = list(lon1 = GRingLongitude1, lat1 = GRingLatitude1, 
                                     lon2 = GRingLongitude2, lat2 = GRingLatitude2, 
                                     lon3 = GRingLongitude3, lat3 = GRingLatitude3, 
                                     lon4 = GRingLongitude4, lat4 = GRingLatitude4,
                                     northbound = NorthBoundingCoord, southbound = SouthBoundingCoord,
                                     eastbound = EastBoundingCoord, westbound = WestBoundingCoord),
                                .f = function(lon1, lat1, lon2, lat2, lon3, lat3, lon4, lat4, northbound, southbound, eastbound, westbound) {
                                  
                                  pt1 <- c(lon1, lat1)
                                  pt2 <- c(lon2, lat2)
                                  pt3 <- c(lon3, lat3)
                                  pt4 <- c(lon4, lat4)
                                  
                                  n_pts <- 10
                                  
                                  

                                  # if(northbound >= 89.9) {
                                  #   mat <- st_sfc(st_multipoint(matrix(rbind(pt1, pt2, pt3, pt4, pt1), ncol = 2)), crs = 4326)
                                  # } else if(southbound <= -89.9) {
                                  #   pt3.1 <- c(180, -90)
                                  #   pt3.2 <- c(-180, -90)
                                  #   mat <- st_sfc(st_multipoint(matrix(rbind(pt1, pt2, pt3, pt3.1, pt3.2, pt4, pt1), ncol = 2)), crs = 4326)
                                  #   
                                  # } else {
                                  #   mat <- st_sfc(st_multipoint(matrix(rbind(pt1, pt2, pt3, pt4, pt1), ncol = 2)), crs = 4326)
                                  # }

                                  mat <- st_sfc(st_multipoint(matrix(rbind(pt1, pt2, pt3, pt4, pt1), ncol = 2)), crs = 4326)
                                  
                                  this_crs <- 4326
                                  
                                  footprint <-
                                    mat %>% 
                                    st_cast("LINESTRING") %>% 
                                    st_transform(this_crs) %>% 
                                    st_segmentize(dfMaxLength = units::set_units(10, km)) %>%
                                    # st_transform(4326) %>% 
                                    st_coordinates() %>% 
                                    st_multipoint() # %>%
                                    # st_cast(to = "MULTIPOLYGON")
                                  # %>% 
                                  #   list() %>% 
                                  #   st_polygon() %>% 
                                  #   st_wrap_dateline(options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180"))
                                  # 
                                  # print(footprint)
                                  # # footprint <-
                                  #   footprint %>%
                                  #   st_linestring()

                                  # footprint <-
                                  #         rbind(
                                  #           pt1,
                                  #           geosphere::gcIntermediate(pt1, pt2, n = n_pts),
                                  #           pt2,
                                  #           geosphere::gcIntermediate(pt2, pt3, n = n_pts),
                                  #           pt3,
                                  #           geosphere::gcIntermediate(pt3, pt4, n = n_pts),
                                  #           pt4,
                                  #           geosphere::gcIntermediate(pt4, pt1, n = n_pts),
                                  #           pt1) %>%
                                  #         list() %>%
                                  #         st_polygon()
                                  
                                  return(footprint)

                                })) %>% st_as_sf(crs = 4326) %>% rowid_to_column(var = "id") %>% 
  st_wrap_dateline(options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180"))
modis_crs <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
north_pole_crs <- "+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
north_pole_crs <- 3995
south_pole_crs <- "+proj=laea +lat_0=-90 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
south_pole_crs <- 3031

increments_per_degree <- 2
increments <- (180 * increments_per_degree) + 1
global_extent <- 
  tibble(lon = c(-180, rep(180, times = increments), rep(-180, increments)), lat = c(90, seq(from = 90, to = -90, length.out = increments), seq(from = -90, to = 90, length.out = increments))) %>% 
  as.matrix() %>% 
  st_multipoint() %>%
  st_sfc(crs = 4326) %>%
  st_cast(to = "LINESTRING")

global_extent <- 
  tibble(lon = c(-180, 180, 180, -180, -180), lat = c(90, 90, -90, -90, 90)) %>% 
  as.matrix() %>% 
  list() %>% 
  st_polygon() %>% 
  st_sfc(crs = 4326) %>% 
  st_sf()


test <- aqua[8, ] %>% st_transform(3031) %>% st_cast("POLYGON")
plot(test %>% st_geometry())

st_coordinates(global_extent) %>% as_tibble() %>% filter(X %in% c(-180, 180))
st_coordinates(aqua[8, ] %>% st_geometry()) %>% as_tibble() %>% filter(X %in% c(-180, 180))
plot(global_extent)

test <- lwgeom::st_split(global_extent, aqua[8, ])

plot(test)
plot(global_extent %>% st_geometry())
plot(aqua[8, ] %>% st_geometry(), add = TRUE)
plot(aqua[8, ] %>% st_cast("POLYGON") %>% st_geometry(), add = TRUE)

plot(test %>% st_geometry())
plot(aqua[1:30, "id"])
plot(aqua[26:28, "id"], axes = TRUE, pal = viridis::viridis(3), lwd = 3)


plot(aqua[17:30, ] %>% st_geometry(), axes = TRUE, lwd = 4)
plot(extent, add = TRUE)


plot(extent)
plot(aqua[1:8, "id"], axes = TRUE, lwd = 4, add = TRUE)
test <- st_cast(aqua, to = "POINT") %>% mutate(point_id = rep(c(1:4, 1), times = nrow(.) %/% 5))
plot(test %>% filter(id >= 8 & id <= 8) %>% dplyr::select(point_id), pal = viridis::viridis(4), pch = 19, cex = 2)

plot(st_join(aqua[8, ], extent))
plot(aqua[8, ] %>% st_geometry())
plot(extent %>% st_geometry(), add = TRUE)
st_coordinates(extent)
coords <- st_coordinates(aqua[8, ])
coords %>% as_tibble() %>% filter(X < -180)

one <- test %>% filter(id == 8)

plot(one[, "point_id"], cex = 2, pch = 19, pal = viridis(4))
mat <- st_coordinates(one)


r_0.25 <- raster::raster("data/data_raw/grid_0_25_degree_vars_modis_D_AFC_num_April_2001.tif")
r_2.5 <- raster::raster("data/data_raw/grid_2_5_degree_vars_modis_D_AFC_num_April_2001.tif")

r_polar <- 
  gdalUtils::gdalwarp(srcfile = "data/data_raw/grid_2_5_degree_vars_modis_D_AFC_num_April_2001.tif", dstfile = "data/data_output/south-pole-stereo_2.5_raster-template.tif", s_srs = proj4string(r_2.5), t_srs = st_crs(3031)$proj4string)
  
r_polar_0.25 <- 
  gdalUtils::gdalwarp(srcfile = "data/data_raw/grid_0_25_degree_vars_modis_D_AFC_num_April_2001.tif", dstfile = "data/data_output/south-pole-stereo_0.25_raster-template.tif", s_srs = proj4string(r_0.25), t_srs = st_crs(3031)$proj4string)

r_polar <- raster::raster("data/data_output/south-pole-stereo_2.5_raster-template.tif")
r_polar <- raster::raster("data/data_output/south-pole-stereo_0.25_raster-template.tif")

r_polar[] <- runif(n = ncell(r_polar))
plot(r_polar)
  raster::projectRaster(from = r_2.5, crs = st_crs(south_pole_crs)$proj4string, res = 2.5)
r <- fasterize::fasterize(sf = test, raster = r_polar, fun = "count")
plot(r)

vals <- raster::values(r)
sum(vals, na.rm = TRUE)
any(!is.na(vals))

plot(test %>% st_geometry(), add = TRUE)
aqua <- st_as_sf(aqua, crs = south_pole_crs)
plot(aqua)
plot(st_geometry(st_as_sf(aqua)))
aqua$SouthBoundingCoord[8]
st_coordinates(aqua[8, ])
aqua %>% dplyr::select(ends_with("BoundingCoord")) %>% slice(8)
aqua$geometry[8]
plot(st_geometry(aqua[8, ]), axes = TRUE)
plot(st_geometry(aqua[7:8, ]), axes = TRUE)
plot(st_geometry(aqua[1:25, ]))
plot(st_geometry(aqua$geometry[8][[1]]))

sf = st_sf(a=1, geom=st_sfc(st_linestring(rbind(c(0,0),c(1,1)))), crs = 4326)
seg = st_segmentize(sf, units::set_units(100, km))
seg = st_segmentize(sf, units::set_units(0.01, rad))


plot(st_geometry(aqua[1:8, ]), axes = TRUE)

plot(st_geometry(aqua[8:9, ]), axes = TRUE)

aqua <-
  aqua %>% 
  dplyr::mutate(geometry = list(tibble(lon = c(GRingLongitude1, GRingLongitude2, GRingLongitude3, GRingLongitude4),
                                       lat = c(GRingLatitude1, GRingLatitude2, GRingLatitude3, GRingLatitude4))))

aqua %>% dplyr::select(1, geometry)
aqua$geometry[1]
# # make object spatial ---------------------------------------------------------------
# 
# # 
# orbit_positions <-
#   data.table::rbindlist(all_orbit_positions)
# 
# modis_crs <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
# 
# orbit_sf <-
#   st_as_sf(orbit_positions, coords = c("lon", "lat"), crs = 4326, remove = FALSE) %>% 
#   st_transform(modis_crs) %>%
#   dplyr::mutate(this_lon = st_coordinates(.)[, 1],
#                 this_lat = st_coordinates(.)[, 2]) %>% 
#   dplyr::arrange(satellite, datetime) %>% 
#   dplyr::mutate(next_lon = lead(this_lon), 
#                 next_lat = lead(this_lat))
# 
# # this_orbit_position <- data.table::copy(orbit_sf)
# # next_orbit_position <- data.table::copy(orbit_sf)
# # this_orbit_position[, `:=`(which_pos = "this", 
# #                            next_lon = NULL, 
# #                            next_lat = NULL,
# #                            lon_sinu = this_lon,
# #                            lat_sinu = this_lat,
# #                            this_lon = NULL,
# #                            this_lat = NULL)]
# # 
# # next_orbit_position[, `:=`(which_pos = "next", 
# #                            this_lon = NULL, 
# #                            this_lat = NULL,
# #                            lon_sinu = next_lon,
# #                            lat_sinu = next_lat,
# #                            next_lon = NULL,
# #                            next_lat = NULL)]
# # 
# # orbit_paths <- 
# #   rbind(this_orbit_position, next_orbit_position)
# # 
# # orbit_paths <- orbit_paths[!is.na(lon_sinu)]
# # 
# # orbit_paths <-
# #   orbit_paths %>% 
# #   st_as_sf(coords = c("lon_sinu", "lat_sinu"), remove = FALSE, crs = modis_crs) %>% 
# #   dtplyr::lazy_dt()
# # 
# # orbit_paths <-
# #   orbit_paths %>% 
# #   dplyr::group_by(id) %>% 
# #   summarize()
# # 
# # install.packages("dtplyr")
# # library(data.table)
# # library(dtplyr)
# # library(dplyr)
# 
# modis_footprint_buffer(orbit_sf[1:10, ])
# ?st_wrap_dateline
# 
# # build bowties around each orbit position ---------------------------------------------------------------
# # 8 minutes to build polygons from points on the Macbook Pro
# 
# dir.create("analyses/analyses_output/modis-footprints", recursive = TRUE)
# 
# (start <- Sys.time())
# 
# aqua_orbits <-
#   orbit_sf %>% 
#   dplyr::filter(satellite == "Aqua") %>% 
#   dplyr::slice(-nrow(.))
# 
# aqua_footprints <- 
#   modis_footprint_buffer(aqua_orbits) %>% 
#   st_wrap_dateline(options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180")) %>% 
#   st_cast("MULTIPOLYGON") %>% 
#   dplyr::mutate(year = year(datetime),
#                 month = month(datetime),
#                 day = day(datetime),
#                 hour = hour(datetime),
#                 minute = minute(datetime))
# 
# sf::st_write(obj = aqua_footprints, dsn = "analyses/analyses_output/modis-footprints/aqua-footprints.gpkg")
# 
# system2(command = "aws", args = "s3 cp analyses/analyses_output/modis-footprints/aqua-footprints.gpkg s3://earthlab-mkoontz/aqua-terra-overpass-corrections/modis-footprints/aqua-footprints.gpkg")
# 
# terra_orbits <-
#   orbit_sf %>% 
#   dplyr::filter(satellite == "Terra") %>% 
#   dplyr::slice(-nrow(.))
# 
# terra_footprints <- 
#   modis_footprint_buffer(terra_orbits) %>% 
#   st_wrap_dateline(options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180")) %>% 
#   st_cast("MULTIPOLYGON") %>% 
#   dplyr::mutate(year = year(datetime),
#                 month = month(datetime),
#                 day = day(datetime),
#                 hour = hour(datetime),
#                 minute = minute(datetime))
# 
# sf::st_write(obj = terra_footprints, dsn = "analyses/analyses_output/modis-footprints/terra-footprints.gpkg")
# 
# system2(command = "aws", args = "s3 cp analyses/analyses_output/modis-footprints/terra-footprints.gpkg s3://earthlab-mkoontz/aqua-terra-overpass-corrections/modis-footprints/terra-footprints.gpkg")
# 
# (Sys.time() - start)
# 
# # rasterize the overlapping image footprints to a regular grid (using one of Joe's as
# # a template)
# r_0.25 <- raster::raster("data/data_raw/grid_0_25_degree_vars_modis_D_AFC_num_April_2001.tif")
# r_2.5 <- raster::raster("data/data_raw/grid_2_5_degree_vars_modis_D_AFC_num_April_2001.tif")
# 
# orbit_overlap_0.25 <- 
#   fasterize::fasterize(sf = sat_footprints, raster = r_0.25, fun = "count")
# orbit_overlap_0.25 <- orbit_overlap_0.25 / (n_periods * 16)
# 
# orbit_overlap_2.5 <- 
#   fasterize::fasterize(sf = sat_footprints, raster = r_2.5, fun = "count")
# orbit_overlap_2.5 <- orbit_overlap_2.5 / (n_periods * 16)
# 
# # visualize
# plot(orbit_overlap_0.25, col = viridis(30))
# plot(st_as_sf(ne_coastline()) %>% st_geometry(), add = TRUE)
# 
# plot(orbit_overlap_2.5, col = viridis(30))
# plot(st_as_sf(ne_coastline()) %>% st_geometry(), add = TRUE)
# 
# # write to disk
# dir.create("analyses/analyses_output")
# writeRaster(x = orbit_overlap_0.25, filename = "analyses/analyses_output/aqua-terra-overpasses-per-day_0.25-degree-grid.tif", overwrite = TRUE)
# writeRaster(x = orbit_overlap_2.5, filename = "analyses/analyses_output/aqua-terra-overpasses-per-day_2.5-degree-grid.tif", overwrite = TRUE)
# 
# # Build a table demonstrating the empirical function that maps latitude to expected number of overpasses
# # per day
# samps_0.25 <-
#   expand.grid(seq(-180, 180, by = 5), seq(-90, 90, by = 0.25)) %>% 
#   setNames(c("lon", "lat")) %>% 
#   as_tibble() %>% 
#   dplyr::mutate(overpasses = extract(x = orbit_overlap_0.25, y = ., method = "bilinear")) %>% 
#   dplyr::filter(!is.na(overpasses))
# 
# samps_2.5 <-
#   expand.grid(seq(-180, 180, by = 5), seq(-90, 90, by = 2.5)) %>% 
#   setNames(c("lon", "lat")) %>% 
#   as_tibble() %>% 
#   dplyr::mutate(overpasses = extract(x = orbit_overlap_2.5, y = ., method = "bilinear")) %>% 
#   dplyr::filter(!is.na(overpasses))
# 
# # include the range of observed overpasses as a minimum and maximum attribute
# overpass_corrections_0.25 <- 
#   samps_0.25 %>%
#   group_by(lat) %>% 
#   summarize(mean_overpasses = mean(overpasses),
#             min_overpasses = min(overpasses),
#             max_overpasses = max(overpasses))
# 
# overpass_corrections_2.5 <- 
#   samps_2.5 %>%
#   group_by(lat) %>% 
#   summarize(mean_overpasses = mean(overpasses),
#             min_overpasses = min(overpasses),
#             max_overpasses = max(overpasses))
# 
# # write to disk
# write.csv(overpass_corrections_0.25, file = "analyses/analyses_output/aqua-terra-overpass-corrections-table_0.25-degree-grid.csv", row.names = FALSE)
# write.csv(overpass_corrections_2.5, file = "analyses/analyses_output/aqua-terra-overpass-corrections-table_2.5-degree-grid.csv", row.names = FALSE)
# 
# # save the visualization to disk
# png("figures/aqua-terra-overpass-corrections-map_0.25-degree-grid.png")
# plot(orbit_overlap_0.25, col = viridis(30))
# plot(st_as_sf(ne_coastline()) %>% st_geometry(), add = TRUE)
# dev.off()
# 
# png("figures/aqua-terra-overpass-corrections-map_2.5-degree-grid.png")
# plot(orbit_overlap_2.5, col = viridis(30))
# plot(st_as_sf(ne_coastline()) %>% st_geometry(), add = TRUE)
# dev.off()
# 
# # save the empirical model plot to disk
# ggplot(overpass_corrections_0.25 %>% filter(lat %in% c(seq(-83.5, -70, by = 0.25), -69:69, seq(70, 83.5, by = 0.25))), aes(x = lat, y = mean_overpasses)) +
#   geom_point(cex = 0.3) +
#   theme_bw() +
#   geom_ribbon(aes(ymin = min_overpasses, ymax = max_overpasses), fill = "red", alpha = 0.1)
# ggsave("figures/aqua-terra-overpass-corrections-function_0.25-degree-grid.png")
# 
# ggplot(overpass_corrections_2.5 %>% filter(lat %in% c(seq(-83.5, -70, by = 0.25), -69:69, seq(70, 83.5, by = 0.25))), aes(x = lat, y = mean_overpasses)) +
#   geom_point(cex = 0.3) +
#   theme_bw() +
#   geom_ribbon(aes(ymin = min_overpasses, ymax = max_overpasses), fill = "red", alpha = 0.1)
# ggsave("figures/aqua-terra-overpass-corrections-function_2.5-degree-grid.png")

