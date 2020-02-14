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

api_keys <- read.csv("data/data_raw/LAADS-DAAC_api-keys.csv", stringsAsFactors = FALSE)

aqua_years <- 2002:2019
terra_years <- 2000:2019

lapply(terra_years[-(1:2)], FUN = function(this_year) {
  system2(command = "wget", args = paste0("-e robots=off -m -np -R .html,.tmp -nH --cut-dirs=3 'https://ladsweb.modaps.eosdis.nasa.gov/archive/geoMeta/61/TERRA/", this_year, "/' --header 'Authorization: Bearer ",  api_keys[api_keys$satellite == "Terra", "key"],"' -P data/data_output/MODIS-footprints/"))
})

lapply(aqua_years, FUN = function(this_year) {
  system2(command = "wget", args = paste0("-e robots=off -m -np -R .html,.tmp -nH --cut-dirs=3 'https://ladsweb.modaps.eosdis.nasa.gov/archive/geoMeta/61/AQUA/", this_year, "/' --header 'Authorization: Bearer ",  api_keys[api_keys$satellite == "Aqua", "key"],"' -P data/data_output/MODIS-footprints/"))
})


# # make object spatial ---------------------------------------------------------------
# 
# # modis_crs <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
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

