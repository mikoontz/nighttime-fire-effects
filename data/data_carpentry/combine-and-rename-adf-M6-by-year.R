# Purpose: Stitch all the active fire detections into the same shapefile

library(tidyverse)
library(sf)
library(lubridate)

shps <- list.files("data/data_raw/fire_archive_M6/", recursive = TRUE, full.names = TRUE, pattern = "*.shp")

shps_list <-
  map(shps, st_read)

shps_list[[1]]

for (i in 1:length(shps_list)) {
  year <- year(ymd(shps_list[[i]][1, ]$ACQ_DATE))
  
  dir.create(paste0("data/data_output/active-fire-detections_M6_global_20001101-20190426/afd_M6_global_", year))
  st_write(shps_list[[i]], paste0("data/data_output/active-fire-detections_M6_global_20001101-20190426/afd_M6_global_", year, "/afd_M6_global_", year, ".shp"))
           
}
lapply(shps_list, function(x) {unique(x$TYPE)})

shps_list[[19]]$TYPE <- NA
shps_list[[19]] <- shps_list[[19]] %>% dplyr::select(everything(), geometry)
lapply(shps_list, colnames)

(start <- Sys.time())
afd_all <- do.call("rbind", shps_list)
dir.create("data/data_output/active-fire-detections_M6_global_20001101-20190426/afd_M6_global_all")
st_write(afd_all, "data/data_output/active-fire-detections_M6_global_20001101-20190426/afd_M6_global_all/afd_M6_global_all.shp")
(Sys.time() - start)