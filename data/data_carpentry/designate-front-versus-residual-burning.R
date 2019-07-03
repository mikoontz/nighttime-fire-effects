# Purpose: designate active fire detections as "front burning" or "residual burning"

library(tidyverse)
library(sf)
library(lubridate)
library(purrr)

firepoints3310 <- st_read("data/data_output/sierra-ypmc-active-fire-detections_epsg3310.gpkg")

frap_meta <- st_read("data/data_raw/wildfire-severity_sierra-nevada-ca-usa_ypmc_1984-2017_fire-metadata.geoJSON", stringsAsFactors = FALSE) %>% st_transform(3310)

frap_meta_post_2000 <- 
  frap_meta %>% 
  tidyr::unite(col = "alarm_date", alarm_year, alarm_month, alarm_day, remove = FALSE) %>% 
  dplyr::mutate(alarm_date = ymd(alarm_date)) %>% 
  dplyr::filter(alarm_date >= ymd("2000-11-01"))

MODIS_temporal_cluster_constant <- 11
afd <- vector(length = nrow(frap_meta_post_2000), mode = "list")

for (i in seq_along(frap_meta_post_2000$fire_id)) {
  
  this_fire_meta <- 
    frap_meta_post_2000[i, ] %>% 
    unite(col = "alarm_date", alarm_year, alarm_month, alarm_day, sep = "-", remove = FALSE) %>% 
    dplyr::mutate(alarm_date = ymd(alarm_date))
  
  afd[[i]] <-
    firepoints3310[this_fire_meta, ] %>% 
    dplyr::filter(year == this_fire_meta$alarm_year) %>% 
    dplyr::filter(ymd(ACQ_DATE) >= this_fire_meta$alarm_date - MODIS_temporal_cluster_constant,
                  ymd(ACQ_DATE) <= this_fire_meta$alarm_date + period(num = 1, units = "year") + MODIS_temporal_cluster_constant) %>% 
    dplyr::mutate(acq = ymd_hm(paste(ACQ_DATE, ACQ_TIME))) %>% 
    dplyr::arrange(acq)
  
  frap_meta_post_2000$active_fire_count[i] <- nrow(afd[[i]])
  frap_meta_post_2000$active_fire_count_day[i] <- nrow(afd[[i]] %>% dplyr::filter(DAYNIGHT == "D"))
  frap_meta_post_2000$active_fire_count_night[i] <- nrow(afd[[i]] %>% dplyr::filter(DAYNIGHT == "N"))
  
  
  if (nrow(afd[[i]]) > 0) {
    
    acq_sequence <- unique(afd[[i]]$acq)
    
    last_perim <- afd[[i]] %>% filter(acq == acq_sequence[1]) %>% st_union()
    
    acq_afd_details <- vector(mode = "list", length = length(acq_sequence))
    
    acq_afd_details[[1]] <- 
      afd[[i]] %>% 
      filter(acq == acq_sequence[1]) %>% 
      st_drop_geometry() %>% 
      dplyr::select(pt_id) %>%
      dplyr::mutate(residual_overlap_m2 = 0,
                    residual_overlap_pct = 0,
                    dist_to_last_perim = NA,
                    days_since_last_acq = NA)
    
    for (j in seq_along(acq_sequence)[-1]) {
      
      this_acq_afd_sf <- 
        afd[[i]] %>% 
        filter(acq == acq_sequence[j]) %>% # filter to just the AFDs from a single acquisition date/time
        dplyr::mutate(residual_overlap_m2 = ifelse(st_intersects(., last_perim, sparse = FALSE)[, 1],
                                                   yes = NA,
                                                   no = 0),
                      residual_overlap_pct = ifelse(st_intersects(., last_perim, sparse = FALSE)[, 1],
                                                    yes = NA,
                                                    no = 0),
                      dist_to_last_perim = ifelse(st_intersects(., last_perim, sparse = FALSE)[, 1],
                                                  yes = NA,
                                                  no = st_distance(., last_perim))) %>% 
        dplyr::mutate(days_since_last_acq = time_length(acq - acq_sequence[j - 1], unit = "days")) 
      
      overlapping_polys <- 
        this_acq_afd_sf %>% 
        st_intersection(last_perim) %>% 
        dplyr::mutate(area = as.numeric(st_area(.))) %>% 
        st_drop_geometry() %>% 
        dplyr::select(pt_id, area) 
      
      this_acq_afd_details <-
        this_acq_afd_sf %>% 
        dplyr::left_join(overlapping_polys,
                         by = "pt_id") %>% 
        dplyr::mutate(residual_overlap_m2 = ifelse(is.na(area), yes = residual_overlap_m2, no = area)) %>% 
        dplyr::mutate(residual_overlap_pct = residual_overlap_m2 / as.numeric(st_area(.))) %>% 
        dplyr::select(pt_id, residual_overlap_m2, residual_overlap_pct, dist_to_last_perim, days_since_last_acq) %>% 
        st_drop_geometry()
      
      acq_afd_details[[j]] <- this_acq_afd_details

      last_perim <- st_union(this_acq_afd_sf) %>% st_union(last_perim)
      
    } # End inner for loop that iterates through the different date/times of active fire detection
    
    acq_afd_details <- 
      do.call("rbind", acq_afd_details) %>%
      dplyr::mutate(fire_id = this_fire_meta$fire_id)
      
    afd[[i]] <- left_join(afd[[i]], acq_afd_details)
    
    afd[[i]]$burn_type <- "NA"
    
    afd[[i]] <-
      afd[[i]] %>% 
      dplyr::mutate(burn_type = ifelse(residual_overlap_pct < 0.5,
                                       yes = "front",
                                       no = "residual"))
    afd[[i]][afd[[i]]$acq == acq_sequence[1], ]$burn_type <- "front"
    
  } # End if statement saying only iterate through different acquisition days if there were at least 1 AFD
  
  print(paste0(i, "...fires processed..."))
} # End loop through all post-November 2011 FRAP fires in database


all_afd = do.call("rbind", afd)

per_daynight_burntype_fire <-
  all_afd %>% 
  st_drop_geometry() %>% 
  group_by(DAYNIGHT, burn_type, fire_id) %>% 
  summarize(FRP = mean(FRP), n = n()) %>%
  left_join(frap_meta_post_2000, by = "fire_id")

ggplot(per_daynight_burntype_fire, aes(x = log(area_ha, base = 10), y = prop, color = DAYNIGHT, lty = burn_type)) +
  geom_point() +
  geom_smooth()

per_daynight_fire <-
  all_afd %>% 
  st_drop_geometry() %>% 
  group_by(DAYNIGHT, fire_id) %>% 
  summarize(n = n()) %>%
  tidyr::spread(key = DAYNIGHT, value = n) %>%
  dplyr::mutate(total = D + N,
                prop_N = N / total) %>%
  left_join(frap_meta_post_2000, by = "fire_id")
  

ggplot(per_daynight_fire, aes(x = gis_acres, y = prop_N, size = total)) +
  geom_point() +
  scale_x_log10() +
  labs(x = "Fire size (acres)",
       y = "Proportion of nighttime detections")

per_daynight_fire %>% group_by(DAYNIGHT) %>% summarize(sum = sum(n))
mean(per_daynight_fire$prop * per_daynight_fire$n)

all_afd %>% st_drop_geometry() %>% group_by(DAYNIGHT, burn_type, year = year(acq)) %>% summarize(FRP = mean(FRP), n = n()) %>% 
  ggplot(aes(x = year, y = n, color = DAYNIGHT, lty = burn_type)) +
  geom_point() +
  geom_smooth()


rim <- 
  all_afd %>% 
  filter(fire_id == "0000e6fc4b09ab014e89") %>% 
  dplyr::rename(geometry = geom) %>% 
  filter(acq < ymd("2013-10-31"))

ggplot(rim, aes(fill = acq)) +
  geom_sf() +
  facet_grid(~burn_type)
