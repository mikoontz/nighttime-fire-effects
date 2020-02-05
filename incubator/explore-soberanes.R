library(tidyverse)
library(sf)
library(USAboundaries)
library(lubridate)
library(fields)

fired <- sf::st_read("data/data_raw/western_hemisphere_to_may2019.gpkg")
ca <- USAboundaries::us_states(resolution = "low", states = "California") %>% 
  sf::st_transform(crs = sf::st_crs(fired))

fired_ca <- fired %>% sf::st_intersection(ca)

soberanes_perim <-
  fired_ca %>% 
  dplyr::filter(start_date >= ymd("2016-07-21") & start_date <= ("2016-07-24")) %>% 
  dplyr::arrange(desc(area_burned_ha)) %>% 
  dplyr::slice(1)

soberanes_viirs <-
  read_csv("data/data_raw/soberanes_viirs.csv") %>% 
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) %>% 
  sf::st_transform(sf::st_crs(soberanes_perim)) %>% 
  sf::st_intersection(soberanes_perim)

# https://en.wikipedia.org/wiki/Solar_zenith_angle
# https://en.wikipedia.org/wiki/Position_of_the_Sun#Declination_of_the_Sun_as_seen_from_Earth
# https://en.wikipedia.org/wiki/Hour_angle

afd <-
  soberanes_viirs %>% 
  dplyr::mutate(acq_hour = as.numeric(substr(acq_time, start = 1, stop = 2)),
                acq_min = as.numeric(substr(acq_time, start = 3, stop = 4)),
                acq_datetime = ymd_hm(paste0(acq_date, " ", acq_hour, ":", acq_min)),
                acq_year = year(acq_datetime),
                acq_month = month(acq_datetime),
                acq_day = day(acq_datetime),
                solar_offset = longitude / 15,
                hemisphere = ifelse(latitude >= 0, yes = "Northern hemisphere", no = "Southern hemisphere"),
                acq_datetime_local = acq_datetime + as.duration(solar_offset * 60 * 60),
                local_doy = lubridate::yday(acq_datetime_local),
                local_hour_decmin = ((acq_hour) + (acq_min / 60) + solar_offset + 24) %% 24,
                local_solar_hour_decmin_round = round(local_hour_decmin),
                local_solar_hour_decmin_round0.5 = round(local_hour_decmin * 2) / 2,
                h = (local_hour_decmin - 12) * 15 * pi / 180,
                phi = latitude * pi / 180,
                delta = -asin(0.39779 * cos(pi / 180 * (0.98565 * (local_doy + 10) + 360 / pi * 0.0167 * sin(pi / 180 * (0.98565 * (local_doy - 2)))))),
                solar_elev_ang = (asin(sin(phi)*sin(delta) + cos(phi)*cos(delta)*cos(h))) * 180 / pi,
                daynight = ifelse(solar_elev_ang > 0, yes = "day", no = "night"))

ggplot(afd, aes(col = daynight)) +
  geom_sf() +
  scale_color_viridis_d(direction = -1)

first_detection <-
  afd %>% 
  dplyr::arrange(acq_datetime) %>% 
  slice(1) %>% 
  pull(acq_datetime)

afd <-
  afd %>% 
  dplyr::mutate(days_elapsed = (acq_datetime - first_detection) / 60 / 60/ 24)

ggplot(afd, aes(col = days_elapsed)) +
  geom_sf() +
  scale_color_viridis_c() +
  facet_grid(cols = vars(daynight))

afd %>% group_by(acq_datetime) %>% 
  summarize(solar_angle = mean(solar_elev_ang))

r <- raster::raster(soberanes_perim)
tps <- Tps(x = st_coordinates(afd), Y = as.numeric(as.character(afd$days_elapsed)))

p <- interpolate(r, tps) %>% mask(r)

plot(p)

# soberanes_modis <-
#   read_csv("data/data_raw/soberanes_modis.csv") %>% 
#   sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% 
#   sf::st_transform(sf::st_crs(soberanes_perim)) %>% 
#   sf::st_intersection(soberanes_perim)
# 
# soberanes_modis <-
#   soberanes_modis %>% 
#   dplyr::mutate(hour = as.numeric(substr(acq_time, start = 1, stop = 2)),
#                 minute = as.numeric(substr(acq_time, start = 3, stop = 4)))
# 
# 
# 
# # plot(st_geometry(ca))
# # plot(soberanes_perim$geom, add = TRUE, col = "red")
# 
# plot(soberanes_perim$geom)
# plot(soberanes_viirs[, "daynight"], cex = 0.5, add = TRUE, pch = 19, legend = TRUE)
# 
# acq_times_viirs <-
#   soberanes_viirs %>% 
#   group_by(acq_date, acq_time) %>% 
#   tally() %>% 
#   dplyr::mutate(sat = "viirs")
# 
# ggplot(acq_times_viirs %>% ungroup() %>% slice(1:2), aes(col = as.factor(acq_time))) +
#   geom_sf()
# 
# acq_times_modis <-
#   soberanes_modis %>% 
#   group_by(acq_date, acq_time) %>% 
#   tally() %>% 
#   dplyr::mutate(sat = "modis")
# 
# acq_times <- 
#   rbind(acq_times_viirs, acq_times_modis) %>% 
#   dplyr::select(acq_date, acq_time, sat, n) %>% 
#   dplyr::arrange(acq_date, acq_time)
# 
# acq_times
# 
