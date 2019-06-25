# Purpose: work through all fires in Sierra YPMC database to get detailed information about
# fire effects and active fire detections within known fire perimeters

library(sf)
library(tidyverse)
library(purrr)

library(raster)
library(fasterize)

library(lubridate)
library(viridis)
library(foreach)
library(doParallel)

library(rasterVis)

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
  fire_raster_meta3310$active_fire_count_day[i] <- nrow(spatial_hits3310[[i]] %>% dplyr::filter(DAYNIGHT == "D"))
  fire_raster_meta3310$active_fire_count_night[i] <- nrow(spatial_hits3310[[i]] %>% dplyr::filter(DAYNIGHT == "N"))
}

# Work through all fires (with active fire hits) ------------------

j = 1

fires_with_active_fire_detections <- which(lapply(spatial_hits3310, FUN = "nrow") %>% do.call("c", .) > 0)

(start <- Sys.time())

#setup parallel backend to use many processors
cores <- detectCores()
cl <- makeCluster(cores[1] - 4) #not to overload your computer
registerDoParallel(cl)

# data_list <- vector(mode = "list", length = length(spatial_hits3310))

# for (j in seq_along(spatial_hits3310)) {
full_dataframe <- foreach (j = fires_with_active_fire_detections, .combine = rbind) %dopar% {
  
  library(sf)
  library(tidyverse)
  library(purrr)
  
  library(raster)
  library(fasterize)
  
  library(lubridate)
  
  this_meta <- fire_raster_meta3310[j, ]
  
  this_severity <- 
    raster::brick(fire_raster_filenames[j]) %>% 
    setNames(band_names)
  
  # background raster matching the footprint of the severity raster, but with all values as 0
  bg_0_raster <- 
    this_severity$rbr %>% 
    raster::setValues(values = 0) %>% 
    raster::mask(this_severity$rbr)
  
  # the count of daytime active fire detections will be 0 if there are none
  this_day_counts_raster <- 
    bg_0_raster %>% 
    setNames("D")
  
  # the count of nighttime active fire detections will be 0 if there are none
  this_night_counts_raster <-
    bg_0_raster %>% 
    setNames("N")
  
  # background raster matching the footprint of the severity raster, but with all values as NA
  bg_NA_raster <-
    this_severity$rbr %>%
    raster::setValues(values = NA)
  
  # All these layers will be NA if there are no active fire detections within the fire perimeter
  this_day_sumFRP_raster <- bg_NA_raster
  this_day_maxFRP_raster <- bg_NA_raster
  this_day_meanFRP_raster <- bg_NA_raster
  this_day_firstACQ_raster <- bg_NA_raster
  this_day_lastACQ_raster <- bg_NA_raster
  this_night_sumFRP_raster <- bg_NA_raster
  this_night_maxFRP_raster <- bg_NA_raster
  this_night_meanFRP_raster <- bg_NA_raster
  this_night_firstACQ_raster <- bg_NA_raster
  this_night_lastACQ_raster <- bg_NA_raster
  this_pt_id <- bg_NA_raster %>% setNames("pt_id")
  
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
    
    this_firstACQ_raster <- 
      fasterize(sf = this_active_fire, raster = this_severity$rbr, fun = "min", background = NA, field = "ACQ", by = "DAYNIGHT") %>% 
      raster::mask(this_severity$rbr)
    
    this_lastACQ_raster <- 
      fasterize(sf = this_active_fire, raster = this_severity$rbr, fun = "max", background = NA, field = "ACQ", by = "DAYNIGHT") %>% 
      raster::mask(this_severity$rbr)
    
    this_meanFRP_raster <- this_sumFRP_raster / this_counts_raster
    
    this_total_counts_raster <- raster::calc(this_counts_raster, fun = sum)
    
    # this_pt_id[this_total_counts_raster != 1] <- NA
    
    if("D" %in% names(this_counts_raster)) {
      this_day_counts_raster <- this_day_counts_raster + this_counts_raster$D
      this_day_sumFRP_raster <- this_sumFRP_raster$D
      this_day_maxFRP_raster <- this_maxFRP_raster$D
      this_day_firstACQ_raster <- this_firstACQ_raster$D
      this_day_lastACQ_raster <- this_lastACQ_raster$D
      this_day_meanFRP_raster <- this_day_sumFRP_raster / this_day_counts_raster
    }
    
    if("N" %in% names(this_counts_raster)) {
      this_night_counts_raster <- this_night_counts_raster + this_counts_raster$N
      this_night_sumFRP_raster <- this_sumFRP_raster$N
      this_night_maxFRP_raster <- this_maxFRP_raster$N
      this_night_firstACQ_raster <- this_firstACQ_raster$N
      this_night_lastACQ_raster <- this_lastACQ_raster$N
      this_night_meanFRP_raster <- this_night_sumFRP_raster / this_night_counts_raster
    }
    
  }
  
  this_data_stack <-
    list(rbr = this_severity$rbr,
         prefire_ndvi = this_severity$prefire_ndvi,
         nbhd_mean_ndvi_1 = this_severity$nbhd_mean_ndvi_1,
         nbhd_sd_ndvi_1 = this_severity$nbhd_sd_ndvi_1,
         prefire_fm100 = this_severity$prefire_fm100,
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
         meanFRP_night = this_night_meanFRP_raster,
         firstACQ_day = this_day_firstACQ_raster,
         firstACQ_night = this_night_firstACQ_raster,
         lastACQ_night = this_night_lastACQ_raster,
         lastACQ_day = this_day_lastACQ_raster) %>% 
    stack()
  
  this_data_stack <-
    this_data_stack %>% 
    rasterToPoints() %>% 
    as.data.frame() %>% 
    st_as_sf(coords = c("x", "y"), crs = 3310, remove = FALSE) %>% 
    dplyr::rename(x_3310 = x, y_3310 = y) %>% 
    dplyr::mutate(dist_to_edge = sf::st_distance(x = ., 
                                                 y = this_meta %>% 
                                                   st_cast("MULTILINESTRING"),
                                                 by_element = TRUE)) %>% 
    dplyr::mutate(intersects_fire_perim = st_intersects(x = ., 
                                                        y = this_meta, 
                                                        sparse = FALSE)[, 1]) %>% 
    dplyr::mutate(dist_to_edge = ifelse(intersects_fire_perim,
                                        yes = dist_to_edge,
                                        no = dist_to_edge * -1)) %>% 
    dplyr::mutate(fire_id = this_meta$fire_id) %>% 
    st_drop_geometry() %>% 
    dplyr::mutate(firstACQ = pmin(firstACQ_day, firstACQ_night, na.rm = TRUE)) %>% 
    dplyr::mutate(lastACQ = pmax(lastACQ_day, lastACQ_night, na.rm = TRUE)) %>% 
    dplyr::mutate(prop_day = ifelse(count_total > 0, yes = prop_day, no = NA),
                  prop_night = ifelse(count_total > 0, yes = prop_night, no = NA)) %>% 
    dplyr::select(fire_id, x_3310, y_3310, ypmc, dist_to_edge, pt_id, count_day, count_night, count_total, firstACQ_day, firstACQ_night, firstACQ, lastACQ_day, lastACQ_night, lastACQ, everything())
  
  this_filename <- paste0("data/data_output/wildfire-severity-active-fire-detections_sierra-nevada-ca-usa_ypmc_1984-2017/", this_meta$date, "_", this_meta$fire_id, "_severity-active-fire-detections.csv")
  
  write_csv(this_data_stack, path = this_filename)
  
  this_data_stack
}

#stop cluster
stopCluster(cl)

(Sys.time() - start) # 49.25071 minutes when using 8 cores (resulting dataframe is 9004105 rows, 26 columns, 380 csv files)

dim(full_dataframe)


