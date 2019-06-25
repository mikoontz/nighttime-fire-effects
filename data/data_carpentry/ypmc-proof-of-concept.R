# Purpose: see what one example fire (Rim Fire 2013) looks like

library(sf)
library(tidyverse)
library(purrr)
library(sp)
library(raster)
library(fasterize)
library(rasterVis)
library(lubridate)
library(viridis)
library(lme4)

firepoints3310 <- st_read("data/data_output/sierra-ypmc-active-fire-detections_epsg3310.gpkg")

frap_meta <- st_read("data/data_raw/wildfire-severity_sierra-nevada-ca-usa_ypmc_1984-2017_fire-metadata.geoJSON", stringsAsFactors = FALSE) %>% st_transform(3310)

frap_meta_post_2000 <- 
  frap_meta %>% 
  tidyr::unite(col = "alarm_date", alarm_year, alarm_month, alarm_day, remove = FALSE) %>% 
  dplyr::mutate(alarm_date = ymd(alarm_date)) %>% 
  dplyr::filter(alarm_date >= ymd("2000-11-01"))


# important metadata for fire effects rasters -----------------------------

band_names <- c('rdnbr', 'prefire_nbr', 'postfire_nbr', 'rdndvi', 'rbr', 'prefire_ndvi', 'postfire_ndvi', 'nbhd_sd_ndvi_1', 'nbhd_mean_ndvi_1', 'nbhd_sd_ndvi_2', 'nbhd_mean_ndvi_2', 'nbhd_sd_ndvi_3', 'nbhd_mean_ndvi_3', 'nbhd_sd_ndvi_4', 'nbhd_mean_ndvi_4', 'date', 'ordinal_day', 'alarm_year', 'alarm_month', 'alarm_day', 'longitude', 'latitude', 'ypmc', 'slope', 'aspect', 'topo_roughness_1', 'topo_roughness_2', 'topo_roughness_3', 'topo_roughness_4', 'elevation', 'B1_prefire', 'B2_prefire', 'B3_prefire', 'B4_prefire', 'B5_prefire', 'B6_prefire', 'B7_prefire', 'B1_postfire', 'B2_postfire', 'B3_postfire', 'B4_postfire', 'B5_postfire', 'B6_postfire', 'B7_postfire', 'prefire_erc', 'prefire_fm100', 'prefire_vpd', 'earlyfire_vs', 'earlyfire_hdw')

# low_sev_lower_rbr_threshold <- best_model$low_sev
low_sev_lower_rbr_threshold <- 0.04509658
# mod_sev_lower_rbr_threshold <- best_model$mod_sev
mod_sev_lower_rbr_threshold <- 0.1125589
# hi_sev_lower_rbr_threshold <- best_model$hi_sev
hi_sev_lower_rbr_threshold <- 0.2823349

# filter fire effects dataset to active fire time period ------------------

fire_ids_post_2000 <-
  frap_meta_post_2000 %>% 
  dplyr::filter(!is.na(fire_id)) %>% 
  pull(fire_id) %>% 
  as.character()

# get the filenames of the different rasters and match to their attributes
fire_raster_meta3310 <- 
  list.files("data/data_raw/wildfire-severity_sierra-nevada-ca-usa_ypmc_1984-2017_rasters/") %>% 
  str_split(pattern = "_", simplify = TRUE) %>% 
  as.data.frame(stringsAsFactors = FALSE) %>% 
  as_tibble() %>% 
  setNames(c("date", "iter", "fire_id", "epsg.tif")) %>% 
  dplyr::filter(fire_id %in% fire_ids_post_2000) %>% 
  tidyr::unite(col = "filename", date, iter, fire_id, epsg.tif, sep = "_", remove = FALSE) %>% 
  dplyr::select(-iter, -epsg.tif) %>% 
  dplyr::left_join(frap_meta_post_2000, by = "fire_id") %>% 
  st_as_sf()

fire_raster_filenames <-
  fire_raster_meta3310 %>% 
  pull(filename) %>% 
  paste0("data/data_raw/wildfire-severity_sierra-nevada-ca-usa_ypmc_1984-2017_rasters/", .)


# spatiotemporal filter of active fire points ------------------------------
# just the points falling within each fire perimeter

fire_raster_meta3310$active_fire_count <- NA

MODIS_temporal_cluster_constant <- 11
spatial_hits3310 <- vector(length = nrow(fire_raster_meta3310), mode = "list")

for (i in seq_along(fire_raster_meta3310$fire_id)) {
  
  this_fire_meta3310 <- 
    fire_raster_meta3310[i, ] %>% 
    unite(col = "alarm_date", alarm_year, alarm_month, alarm_day, sep = "-", remove = FALSE) %>% 
    dplyr::mutate(alarm_date = ymd(alarm_date))
  
  spatial_hits3310[[i]] <-
    firepoints3310[this_fire_meta3310, ] %>% 
    dplyr::filter(year == this_fire_meta3310$alarm_year) %>% 
    dplyr::filter(ymd(ACQ_DATE) >= this_fire_meta3310$alarm_date - MODIS_temporal_cluster_constant,
                  ymd(ACQ_DATE) <= this_fire_meta3310$alarm_date + period(num = 1, units = "year") + MODIS_temporal_cluster_constant)
  
  fire_raster_meta3310$active_fire_count[i] <- nrow(spatial_hits3310[[i]])
}


# testing a single fire ----------------------------------------------------
rim_meta <- fire_raster_meta3310[472, ]
rim_active_fire <- spatial_hits3310[[472]]

ggplot(rim_active_fire, aes(col = DAYNIGHT)) +
  geom_sf()

# Get an example fire severity image --------------------------------------
rim_severity <- 
  raster::brick(fire_raster_filenames[472]) %>% 
  setNames(band_names)

# A severity palette
RBR_viz <- c('#008000', '#ffff00', '#ffA500', '#ff0000')

# breaks <- c(cellStats(rim_severity[["rbr"]], min), seq(low_sev_lower_rbr_threshold, hi_sev_lower_rbr_threshold, length.out = 100), cellStats(rim_severity[["rbr"]], max))
# cols <- colorRampPalette(RBR_viz)
# 
# # Use levelplot() from the rasterVis package to appropriately designate the color ramp
# # across the calibrated severity thresholds
# levelplot(rim_severity[["rbr"]], at = breaks, col.regions = cols)

plot(rim_severity$rbr, col = colorRampPalette(RBR_viz)(100))
plot(rim_active_fire$geometry, add = TRUE)

# rasterize the squares around each active fire count ---------------------
night_raster <- 
  fasterize(sf = rim_active_fire[rim_active_fire$DAYNIGHT == "N", ], raster = rim_severity$rbr, fun = "count", background = 0) %>% 
  raster::mask(rim_meta)

day_raster <- 
  fasterize(sf = rim_active_fire[rim_active_fire$DAYNIGHT == "D", ], raster = rim_severity$rbr, fun = "count", background = 0) %>% 
  raster::mask(rim_meta)

counts_raster <- 
  fasterize(sf = rim_active_fire, raster = rim_severity$rbr, fun = "count", background = 0) %>% 
  raster::mask(rim_meta)


plot(counts_raster)
plot(night_raster)
plot(day_raster)

single_active_fire <- counts_raster
single_active_fire[single_active_fire[] > 1] <- 0
plot(single_active_fire)

single_night_raster <- night_raster * single_active_fire
single_day_raster <- day_raster * single_active_fire

plot(single_night_raster)
plot(single_day_raster)

single_night_rbr <- rim_severity$rbr
single_night_rbr[single_night_raster[] == 0 | is.na(single_night_raster[])] <- NA

single_day_rbr <- rim_severity$rbr
single_day_rbr[single_day_raster[] == 0 | is.na(single_day_raster[])] <- NA

plot(single_night_rbr, col = colorRampPalette(RBR_viz)(100))
plot(single_day_rbr, col = colorRampPalette(RBR_viz)(100))

rbr_vals <- 
  tibble(DAYNIGHT = "N", vals = values(single_night_rbr)) %>% 
  bind_rows(tibble(DAYNIGHT = "D", vals = values(single_day_rbr))) %>% 
  dplyr::filter(!is.na(vals))

ggplot(rbr_vals, aes(x = vals, color = DAYNIGHT)) +
  geom_density()

rbr_vals %>% group_by(DAYNIGHT) %>% summarize(n = n(), mean = mean(vals))
plot(night_raster / counts_raster, col = viridis(100))
plot(day_raster / counts_raster, col = viridis(100))

mean(values(night_raster / counts_raster), na.rm = TRUE)
mean(values(day_raster / counts_raster), na.rm = TRUE)

plot(table(values(day_raster / counts_raster)[values(counts_raster) > 1]), xaxt = "n", xlim = c(0, 1), ylab = "count")
at <- c(0, 1/2, 1/3, 1/4, 1/5, 2/3, 2/5,  3/4, 3/5, 4/5, 1)
axis(at = at, labels = round(at, 2), side = 1, las = 2)
mtext(side = 2, text = "count")

