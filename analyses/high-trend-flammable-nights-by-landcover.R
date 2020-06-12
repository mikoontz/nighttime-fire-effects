library(tidyverse)
library(sf)
library(raster)
library(here)

r <- raster::raster(here::here("data", "data_output", "nightstrend.nc"))
r <- raster::rotate(x = r)

# get landcover data ------------------------------------------------------

# Just use the 11 landcovers that we used to derive our VPDt
landcovers_of_interest <- c(1:2, 4:10, 12, 14)

landcover <- raster::raster(x = here::here("data", "data_raw", "GLDASp4_domveg_025d.nc4"))

lc_lookup <- readr::read_csv(here::here("data", "data_raw", "GLDASp4_domveg_025d_lookup-table.csv"))

s <- stack(landcover, raster::area(landcover))
names(s) <- c("index", "pixel_area")

r <- raster::projectRaster(from = r, to = s)

trend <- raster::stack(s, r)

landcover_area <- 
  as.data.frame(s, xy = TRUE) %>% 
  dplyr::group_by(index) %>% 
  dplyr::summarize(total_area = sum(pixel_area))

trend_df <- 
  as.data.frame(trend, xy = TRUE) %>% 
  dplyr::filter(index %in% landcovers_of_interest) %>% 
  dplyr::filter(trend >= 14) %>% 
  dplyr::as_tibble()

landcover_trends <-
  trend_df %>% 
  dplyr::group_by(index) %>% 
  dplyr::summarize(gt14_area = sum(pixel_area)) %>% 
  dplyr::left_join(landcover_area, by = "index") %>%
  dplyr::mutate(pct_big_trend = gt14_area / total_area) %>% 
  dplyr::left_join(lc_lookup)

overall_trend <-
  trend_df %>% 
  dplyr::group_by(index) %>% 
  dplyr::summarize(gt14_area = sum(pixel_area)) %>% 
  dplyr::left_join(landcover_area, by = "index") %>%
  dplyr::summarize(gt14_area = sum(gt14_area), total_area = sum(total_area)) %>% 
  dplyr::mutate(pct_big_trend = gt14_area / total_area)

