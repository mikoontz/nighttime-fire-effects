library(tidyverse)
library(sf)
library(data.table)

# How much does it matter whether we use the center of the sun's disc to calculate day/night
# (basing on solar elevation angle) vs. using the top of the sun's disc?

years <- 2003:2018

afd <- lapply(X = years, 
              FUN = function(this_year) {
                fread(paste0("data/data_output/mcd14ml_solar-elevation-angle/mcd14ml_solar-elevation-angle_", this_year, ".csv"))
              })

afd <- data.table::rbindlist(afd)

afd[, `:=`(lat_round = round((LATITUDE + 0.125) * 4) / 4 - 0.125,
           lon_round = round((LONGITUDE + 0.125) * 4) / 4 - 0.125)]
afd[, daynight_mid_disc := ifelse(solar_elev_ang > 0, yes = "day", no = "night")]
afd[, daynight_top_disc := ifelse(solar_elev_ang > -(0.53/2), yes = "day", no = "night")]
afd <- afd[TYPE == 0 & CONFIDENCE >= 10]
data.table::setkey(afd, lat_round, lon_round)

afd[, .(pct_solar_ang_gt_0 = length(which(solar_elev_ang > 0)) / length(solar_elev_ang)), by = (DAYNIGHT)]
afd[, .(pct_solar_ang_gt_neg_0.265 = length(which(solar_elev_ang > -0.265)) / length(solar_elev_ang)), by = (DAYNIGHT)]

# > afd[, .(pct_solar_ang_gt_0 = length(which(solar_elev_ang > 0)) / length(solar_elev_ang)), by = (DAYNIGHT)]
# DAYNIGHT pct_solar_ang_gt_0
# 1:        D         1.00000000
# 2:        N         0.02287973
# 
# > afd[, .(pct_solar_ang_gt_neg_0.265 = length(which(solar_elev_ang > -0.265)) / length(solar_elev_ang)), by = (DAYNIGHT)]
# DAYNIGHT pct_solar_ang_gt_neg_0.265
# 1:        D                 1.00000000
# 2:        N                 0.02459592



# landcover ---------------------------------------------------------------

# Just use the 11 landcovers that we used to derive our VPDt
landcovers_of_interest <- c(1:2, 4:10, 12, 14)

landcover <- raster::raster(x = "data/data_raw/GLDASp4_domveg_025d.nc4")
landcover_area <- raster::area(landcover)
s <- stack(landcover, landcover_area)
# raster::origin(landcover) <- raster::origin(raster_template)
landcover_table <- 
  readr::read_csv(file = "data/data_raw/GLDASp4_domveg_025d_lookup-table.csv") %>% 
  dplyr::mutate(landcover_split = str_replace(landcover, pattern = " ", replacement = "\n"))

landcover_df <- 
  s %>% 
  as.data.frame(xy = TRUE) %>% 
  setNames(c("lon", "lat", "index", "pixel_area_km2")) %>% 
  dplyr::left_join(landcover_table, by = "index")

landcover_DT <- as.data.table(landcover_df)
data.table::setkeyv(landcover_DT, cols = c("lon", "lat"))


# aggregate to lon lat ----------------------------------------------------

misclassified_daynight_lon_lat <- 
  afd[, .(solar_elev_gt_0 = length(which(solar_elev_ang > 0)),
          solar_elev_lte_0 = length(which(solar_elev_ang <= 0)),
          total_count = length(solar_elev_ang)), 
      by = .(lon = lon_round, lat = lat_round, DAYNIGHT)]

# misclassified_daynight_lon_lat <- misclassified_daynight_lon_lat[(DAYNIGHT == "D" & solar_elev_lte_0 > 0) | (DAYNIGHT == "N" & solar_elev_gt_0)]

misclassified_count_landcover_lon_lat <- landcover_DT[misclassified_daynight_lon_lat]

misclassified_count_landcover <- 
  misclassified_count_landcover_lon_lat[, .(misclassified_count = ifelse(DAYNIGHT == "D", yes = sum(solar_elev_lte_0), no = sum(solar_elev_gt_0)),
                                                                           total_count = sum(total_count)),
                                                                       by = .(DAYNIGHT, landcover)]

sum(misclassified_count_landcover$misclassified_count) / sum(misclassified_count_landcover$total_count)
misclassified_count_landcover[, pct_misclassified := 100 * misclassified_count / total_count]

misclassified_count_landcover
readr::write_csv(misclassified_count_landcover, path = "tables/misclassified-by-landcover_all.csv")

# DAYNIGHT                          landcover misclassified_count total_count pct_misclassified
# 1:        D         Evergreen Broadleaf Forest                   0    11457925       0.000000000
# 2:        N                              Ocean                 511       57720       0.885308385
# 3:        D                              Ocean                   0      388554       0.000000000
# 4:        D                       Mixed Forest                   0     1945192       0.000000000
# 5:        N                       Mixed Forest                3974      329229       1.207062561
# 6:        D                          Grassland                   0     3605098       0.000000000
# 7:        D                      missing value                   0      355822       0.000000000
# 8:        N                      missing value                 435       68132       0.638466506
# 9:        N                          Grassland                 724      586353       0.123475108
# 10:        D                    Open Shrublands                   0     2824721       0.000000000
# 11:        N                    Open Shrublands               47788      874300       5.465858401
# 12:        D         Deciduous Broadleaf Forest                   0     1307597       0.000000000
# 13:        D                           Savannas                   0    17641307       0.000000000
# 14:        D                  Closed Shrublands                   0       25817       0.000000000
# 15:        N                  Closed Shrublands                   0        9049       0.000000000
# 16:        D                       Snow and Ice                   0        3579       0.000000000
# 17:        N                       Snow and Ice                   1         383       0.261096606
# 18:        D        Evergreen Needleleaf Forest                   0     1022884       0.000000000
# 19:        N         Evergreen Broadleaf Forest                  39     1492101       0.002613764
# 20:        D       Barren or Sparsely Vegetated                   0      103858       0.000000000
# 21:        D                           Cropland                   0     6121365       0.000000000
# 22:        N                           Cropland                  27      521782       0.005174575
# 23:        N        Evergreen Needleleaf Forest               59548      531348      11.206967938
# 24:        D                     Woody Savannas                   0    16760130       0.000000000
# 25:        N                     Woody Savannas                   0     1071021       0.000000000
# 26:        D Cropland/Natural Vegetation Mosaic                   0      457787       0.000000000
# 27:        N                           Savannas                   0     1635745       0.000000000
# 28:        N Cropland/Natural Vegetation Mosaic                   0       77192       0.000000000
# 29:        N         Deciduous Broadleaf Forest                   0      142119       0.000000000
# 30:        D                 Urban and Built-Up                   0      106540       0.000000000
# 31:        N                 Urban and Built-Up                  22       17986       0.122317358
# 32:        D                  Permanent Wetland                   0       32699       0.000000000
# 33:        N                  Permanent Wetland                 300        5271       5.691519636
# 34:        N       Barren or Sparsely Vegetated                   0       79628       0.000000000
# 35:        D                      Wooded Tundra                   0      183297       0.000000000
# 36:        D        Deciduous Needleleaf Forest                   0      397843       0.000000000
# 37:        N        Deciduous Needleleaf Forest               39425      177444      22.218277316
# 38:        N                      Wooded Tundra               31707       77987      40.656776129
# 39:        D                       Mixed Tundra                   0        3126       0.000000000
# 40:        N                       Mixed Tundra                 383        1333      28.732183046
# DAYNIGHT                          landcover misclassified_count total_count pct_misclassified


misclassified_by_landcover_table <- 
  misclassified_count_landcover %>% 
  dplyr::filter(misclassified_count > 0) %>% 
  dplyr::arrange(desc(pct_misclassified))

readr::write_csv(misclassified_by_landcover_table, path = "tables/misclassified-by-landcover_just-categories-with-misclassifications.csv")
# DAYNIGHT                   landcover misclassified_count total_count pct_misclassified
# 1         N               Wooded Tundra               31707       77987      40.656776129
# 2         N                Mixed Tundra                 383        1333      28.732183046
# 3         N Deciduous Needleleaf Forest               39425      177444      22.218277316
# 4         N Evergreen Needleleaf Forest               59548      531348      11.206967938
# 5         N           Permanent Wetland                 300        5271       5.691519636
# 6         N             Open Shrublands               47788      874300       5.465858401
# 7         N                Mixed Forest                3974      329229       1.207062561
# 8         N                       Ocean                 511       57720       0.885308385
# 9         N               missing value                 435       68132       0.638466506
# 10        N                Snow and Ice                   1         383       0.261096606
# 11        N                   Grassland                 724      586353       0.123475108
# 12        N          Urban and Built-Up                  22       17986       0.122317358
# 13        N                    Cropland                  27      521782       0.005174575
# 14        N  Evergreen Broadleaf Forest                  39     1492101       0.002613764


# by latitude -------------------------------------------------------------

misclassified_count_landcover_lon_lat[, `:=`(lat_round = round(lat / 5) * 5)]

misclassified_by_lat <-
  misclassified_count_landcover_lon_lat[, .(misclassified_count = ifelse(DAYNIGHT == "D", yes = sum(solar_elev_lte_0), no = sum(solar_elev_gt_0)),
                                          total_count = sum(total_count)),
                                      by = .(lat = lat_round, DAYNIGHT)]

readr::write_csv(misclassified_by_lat, path = "tables/misclassified-by-lat_all.csv")

misclassified_by_lat_table <-
  misclassified_by_lat %>% 
  dplyr::filter(misclassified_count > 0) %>%
  dplyr::mutate(pct_misclassified = 100 * misclassified_count / total_count)

readr::write_csv(misclassified_by_lat_table, path = "tables/misclassified-by-lat_just-categories-with-misclassifications.csv")
# Latitude rounded to the nearest 5 degree
# lat DAYNIGHT misclassified_count total_count pct_misclassified
# 1  55        N                2063      301062         0.6852409
# 2  60        N               46343      327013        14.1716079
# 3  65        N              129761      294424        44.0728337
# 4  70        N                6717       11666        57.5775759