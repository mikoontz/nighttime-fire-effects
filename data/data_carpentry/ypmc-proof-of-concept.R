# Purpose: repackage the shapefiles to gpkg

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

# sn <- st_read("data/data_raw/jepson_sierra-nevada-ecoregion/jepson_sierra-nevada-ecoregion.shp")
# sn3310 <- st_transform(sn, 3310)
# sn_raster <- 
#   sn3310 %>% 
#   raster(resolution = 1000)

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
rim_active_fire4326 <- 
  firepoints %>% 
  dplyr::filter(pt_id %in% rim_active_fire$pt_id)

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

# Work through all fires with active fire hits ----------------------------

frap_fires_with_modis_hits <- which(lapply(spatial_hits3310, nrow) %>% do.call("c", .) != 0)

j = frap_fires_with_modis_hits[1]

(start <- Sys.time())
all_rbr_vals <- NULL

for (j in frap_fires_with_modis_hits) {
  this_meta <- fire_raster_meta3310[j, ]
  
  this_severity <- 
    raster::brick(fire_raster_filenames[j]) %>% 
    setNames(band_names)
  
  bg_0_raster <- 
    this_severity$rbr %>% 
    raster::setValues(values = 0) %>% 
    raster::mask(this_severity$rbr)
  
  bg_NA_raster <-
    this_severity$rbr %>%
    raster::setValues(values = NA)
  
  this_day_sumFRP_raster <- bg_NA_raster
  this_day_maxFRP_raster <- bg_NA_raster
  this_day_meanFRP_raster <- bg_NA_raster
  this_day_latestACQ_raster <- bg_NA_raster
  this_night_sumFRP_raster <- bg_NA_raster
  this_night_maxFRP_raster <- bg_NA_raster
  this_night_meanFRP_raster <- bg_NA_raster
  this_night_latestACQ_raster <- bg_NA_raster
  this_pt_id <- bg_NA_raster %>% setNames("pt_id")
  
  this_day_counts_raster <- 
    bg_0_raster %>% 
    setNames("D")
  
  this_night_counts_raster <-
    bg_0_raster %>% 
    setNames("N")
  
  this_active_fire <- spatial_hits3310[[j]]
  
  if(nrow(this_active_fire) > 0) {
    
    # Put acquisition date/time into a single column
    this_active_fire <-
      this_active_fire %>% 
      dplyr::mutate(ACQ = ymd_hm(paste(ACQ_DATE, ACQ_TIME, sep = "-")))
    
    # rasterize the squares around each active fire count ---------------------
    # Count the number of day and night active fire detections
    # Use 0 as the background number
    # Then mask out all the Landsat pixels outside the fire perimeter
    this_counts_raster <- 
      fasterize(sf = this_active_fire, raster = this_severity$rbr, fun = "count", background = 0, field = NULL, by = "DAYNIGHT") %>% 
      raster::mask(this_severity$rbr)
    
    # this_pt_id <- fasterize(sf = this_active_fire, raster = this_severity$rbr, fun = "first", field = "pt_id", by = "DAYNIGHT") %>% 
    #   raster::mask(this_severity$rbr)

    this_pt_id <- fasterize(sf = this_active_fire, raster = this_severity$rbr, fun = "first", field = "pt_id") %>%
      raster::mask(this_severity$rbr)

    this_sumFRP_raster <- 
      fasterize(sf = this_active_fire, raster = this_severity$rbr, fun = "sum", background = NA, field = "FRP", by = "DAYNIGHT") %>% 
      raster::mask(this_severity$rbr)
    
    this_maxFRP_raster <- 
      fasterize(sf = this_active_fire, raster = this_severity$rbr, fun = "max", background = NA, field = "FRP", by = "DAYNIGHT") %>% 
      raster::mask(this_severity$rbr)
    
    this_latestACQ_raster <- 
      fasterize(sf = this_active_fire, raster = this_severity$rbr, fun = "max", background = NA, field = "ACQ", by = "DAYNIGHT") %>% 
      raster::mask(this_severity$rbr)
    
    this_meanFRP_raster <- this_sumFRP_raster / this_counts_raster
    
    this_total_counts_raster <- raster::calc(this_counts_raster, fun = sum)
    
    # this_pt_id[this_total_counts_raster != 1] <- NA
    
    if(names(this_counts_raster) %in% "D") {
      this_day_counts_raster <- this_day_counts_raster + this_counts_raster$D
      this_day_sumFRP_raster <- this_sumFRP_raster$D
      this_day_maxFRP_raster <- this_maxFRP_raster$D
      this_day_latestACQ_raster <- this_latestACQ_raster$D
      this_day_meanFRP_raster <- this_day_sumFRP_raster / this_day_counts_raster
    }
    
    if(names(this_counts_raster) %in% "N") {
      this_night_counts_raster <- this_night_counts_raster + this_counts_raster$N
      this_night_sumFRP_raster <- this_sumFRP_raster$N
      this_night_maxFRP_raster <- this_maxFRP_raster$N
      this_night_latestACQ_raster <- this_latestACQ_raster$N
      this_night_meanFRP_raster <- this_night_sumFRP_raster / this_night_counts_raster
    }

  }
  this_data_stack <-
    list(rbr = this_severity$rbr,
         pt_id = this_pt_id,
         ypmc = this_severity$ypmc,
         count_day = this_day_counts_raster,
         count_night = this_night_counts_raster,
         count_total = this_day_counts_raster + this_night_counts_raster,
         prop_day = this_day_counts_raster / (this_day_counts_raster + this_night_counts_raster),
         prop_night = this_night_counts_raster / (this_day_counts_raster + this_night_counts_raster),
         sumFRP_day = this_day_sumFRP_raster,
         sumFRP_night = this_night_sumFRP_raster,
         maxFRP_day = this_day_maxFRP_raster,
         maxFRP_night = this_night_maxFRP_raster,
         meanFRP_day = this_day_meanFRP_raster,
         meanFRP_night = this_night_meanFRP_raster) %>% 
    stack()
  
  this_data_stack <-
    this_data_stack %>% 
    rasterToPoints() %>% 
    as.data.frame() %>% 
    st_as_sf(coords = c("x", "y"), crs = 3310, remove = FALSE) %>% 
    dplyr::mutate(dist_to_edge = sf::st_distance(x = ., 
                                                 y = this_meta %>% 
                                                   st_cast("MULTILINESTRING"),
                                                 by_element = TRUE)) %>% 
    dplyr::mutate(intersects = st_intersects(x = ., 
                                             y = this_meta, 
                                             sparse = FALSE)[, 1]) %>% 
    dplyr::mutate(dist_to_edge = ifelse(intersects,
                                        yes = dist_to_edge,
                                        no = dist_to_edge * -1)) %>% 
    dplyr::mutate(fire_id = this_meta$fire_id)
}
(Sys.time() - start)


all_spatial_hits <- do.call("rbind", spatial_hits3310)

rbr_vals_per_active_fire_hit <- 
  all_rbr_vals %>% 
  left_join(all_spatial_hits, by = c("DAYNIGHT", "pt_id"))

write_csv(rbr_vals_per_active_fire_hit, "data/data_output/rbr-vals-per-active-fire-hit.csv")

ypmc <-
  rbr_vals_per_active_fire_hit %>% filter(ypmc == 1)

ggplot(ypmc, aes(x = FRP, y = rbr)) + geom_point(alpha = 0.1) + geom_smooth(aes(color = DAYNIGHT))

ggplot(ypmc, aes(x = FRP, color = DAYNIGHT)) + geom_density()

ggplot(ypmc, aes(x = BRIGHTNESS, y = rbr)) + geom_point(alpha = 0.1) + geom_smooth(aes(color = DAYNIGHT))

ggplot(ypmc, aes(x = BRIGHT_T31, y = rbr)) + geom_point(alpha = 0.1) + geom_smooth(aes(color = DAYNIGHT))

ypmc %>% 
  group_by(fire_id, DAYNIGHT, pt_id) %>% 
  summarize(mean_rbr = mean(rbr)) %>% 
  summarize(mean_rbr = mean(mean_rbr)) %>% 
  spread(key = DAYNIGHT, value = mean_rbr) %>% 
  mutate(diff = D - N) %>% 
  filter(!is.na(diff)) %>% 
  ungroup() %>% 
  summarize(mean_diff = mean(diff))

ggplot(ypmc, aes(x = DAYNIGHT, y = rbr)) +
  geom_boxplot() +
  labs(x = "Day or night",
       y = "RBR")

ypmc %>% 
  group_by(pt_id, DAYNIGHT) %>% 
  summarize(rbr = mean(rbr)) %>% 
  ggplot(aes(x = DAYNIGHT, y = rbr)) +
  geom_boxplot() +
  labs(x = "Day or night",
       y = "RBR (mean per active fire pixel)")

# test <- 
#   all_rbr_vals %>% 
#   dplyr::filter(ypmc == 1) %>% 
#   dplyr::mutate(high_sev = ifelse(rbr > hi_sev_lower_rbr_threshold, yes = 1, no = 0)) %>% 
#   group_by(fire_id, DAYNIGHT) %>% 
#   summarize(n = n(),
#             mean_rbr = mean(rbr),
#             prop_high_sev = mean(high_sev)) %>% 
#    dplyr::ungroup() %>% 
#   dplyr::group_by(fire_id, DAYNIGHT) %>% 
#   dplyr::summarize(n = n(),
#                    prop_high_sev = mean(prop_high_sev),
#                    mean_rbr = mean(mean_rbr),
#                    med_rbr = median(mean_rbr),
#                    quant10 = quantile(mean_rbr, probs = 0.1),
#                    quant90 = quantile(mean_rbr, probs = 0.9)) %>% 
#   dplyr::left_join(fire_raster_meta3310, by = "fire_id") %>% 
#   dplyr::filter(!is.na(objective)) %>% 
#   dplyr::mutate(objective = ifelse(objective == 1, yes = "suppression", no = "wfu"))
#   
# diff_prop_high_sev_per_event <-
#   test %>% 
#   dplyr::select(fire_id, DAYNIGHT, prop_high_sev) %>% 
#   tidyr::spread(key = DAYNIGHT, value = prop_high_sev) %>% 
#   dplyr::mutate(diff = D - N) %>% 
#   dplyr::filter(!is.na(diff)) %>% 
#   dplyr::left_join(fire_raster_meta3310, by = "fire_id") %>% 
#   dplyr::filter(!is.na(objective)) %>% 
#   dplyr::mutate(objective = ifelse(objective == 1, yes = "suppression", no = "wfu"))
# 
# diff_mean_rbr_per_event <-
#   test %>% 
#   dplyr::select(fire_id, DAYNIGHT, mean_rbr) %>% 
#   tidyr::spread(key = DAYNIGHT, value = mean_rbr) %>% 
#   dplyr::mutate(diff = D - N) %>% 
#   dplyr::filter(!is.na(diff)) %>% 
#   dplyr::left_join(fire_raster_meta3310, by = "fire_id") %>% 
#   dplyr::filter(!is.na(objective)) %>% 
#   dplyr::mutate(objective = ifelse(objective == 1, yes = "suppression", no = "wfu"))
# 
# diff_med_rbr_per_event <-
#   test %>% 
#   dplyr::select(fire_id, DAYNIGHT, med_rbr) %>% 
#   tidyr::spread(key = DAYNIGHT, value = med_rbr) %>% 
#   dplyr::mutate(diff = D - N) %>% 
#   dplyr::filter(!is.na(diff)) %>% 
#   dplyr::left_join(fire_raster_meta3310, by = "fire_id") %>% 
#   dplyr::filter(!is.na(objective)) %>% 
#   dplyr::mutate(objective = ifelse(objective == 1, yes = "suppression", no = "wfu"))
# 
# diff_quant10_rbr_per_event <-
#   test %>% 
#   dplyr::select(fire_id, DAYNIGHT, quant10) %>% 
#   tidyr::spread(key = DAYNIGHT, value = quant10) %>% 
#   dplyr::mutate(diff = D - N) %>% 
#   dplyr::filter(!is.na(diff)) %>% 
#   dplyr::left_join(fire_raster_meta3310, by = "fire_id") %>% 
#   dplyr::filter(!is.na(objective)) %>% 
#   dplyr::mutate(objective = ifelse(objective == 1, yes = "suppression", no = "wfu"))
# 
# diff_quant90_rbr_per_event <-
#   test %>% 
#   dplyr::select(fire_id, DAYNIGHT, quant90) %>% 
#   tidyr::spread(key = DAYNIGHT, value = quant90) %>% 
#   dplyr::mutate(diff = D - N) %>% 
#   dplyr::filter(!is.na(diff)) %>% 
#   dplyr::left_join(fire_raster_meta3310, by = "fire_id") %>% 
#   dplyr::filter(!is.na(objective)) %>% 
#   dplyr::mutate(objective = ifelse(objective == 1, yes = "suppression", no = "wfu"))
# 
# diff_prop_high_sev_per_event %>% group_by(objective) %>% summarize(mean(diff))
# diff_mean_rbr_per_event %>% group_by(objective) %>% summarize(mean(diff))
# diff_med_rbr_per_event %>% group_by(objective) %>% summarize(mean(diff))
# diff_quant10_rbr_per_event %>% group_by(objective) %>% summarize(mean(diff))
# diff_quant90_rbr_per_event %>% group_by(objective) %>% summarize(mean(diff))
# 
# 
# plot(density(diffs_per_event$diff))
# mean(diffs_per_event$diff)
