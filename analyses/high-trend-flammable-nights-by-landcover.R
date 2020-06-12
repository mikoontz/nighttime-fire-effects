library(tidyverse)
library(sf)
library(raster)
library(here)
library(gt)

# https://climate.northwestknowledge.net/ACSL/NIGHTFIRE/nightstrend.nc

if(!file.exists(here::here("data", "data_output", "nightstrend.nc"))) {
  if(!dir.exists(here::here("data", "data_output"))) {
    dir.create(here::here("data", "data_output"), recursive = TRUE)
  }
  download.file(url = "https://climate.northwestknowledge.net/ACSL/NIGHTFIRE/nightstrend.nc",
                destfile = here::here("data", "data_output", "nightstrend.nc"),
                method = "curl")
}

r <- raster::raster(here::here("data", "data_output", "nightstrend.nc"))
r <- raster::rotate(x = r)

# get landcover data ------------------------------------------------------

# Just use the 11 landcovers that we used to derive our VPDt
landcovers_of_interest <- c(1:2, 4:10, 12, 14)

# https://ldas.gsfc.nasa.gov/sites/default/files/ldas/gldas/VEG/GLDASp4_domveg_025d.nc4
if(!file.exists(here::here("data", "data_raw", "GLDASp4_domveg_025d.nc4"))) {
  if(!dir.exists(here::here("data", "data_raw"))) {
    dir.create(here::here("data", "data_raw"), recursive = TRUE)
  }
  download.file(url = "https://ldas.gsfc.nasa.gov/sites/default/files/ldas/gldas/VEG/GLDASp4_domveg_025d.nc4",
                method = "curl",
                destfile = here::here("data", "data_raw", "GLDASp4_domveg_025d.nc4"))
}

landcover <- raster::raster(x = here::here("data", "data_raw", "GLDASp4_domveg_025d.nc4"))

# From https://ldas.gsfc.nasa.gov/gldas/vegetation-class-mask
read_tsv("Index 	Vegetation Type
0 	missing value
1 	Evergreen Needleleaf Forest 
2 	Evergreen Broadleaf Forest
3 	Deciduous Needleleaf Forest 
4 	Deciduous Broadleaf Forest
5 	Mixed Forest 
6 	Closed Shrublands 
7 	Open Shrublands 
8 	Woody Savannas
9 	Savannas
10 	Grassland
11 	Permanent Wetland
12 	Cropland
13 	Urban and Built-Up
14 	Cropland/Natural Vegetation Mosaic
15 	Snow and Ice 
16 	Barren or Sparsely Vegetated 
17 	Ocean 
18 	Wooded Tundra 
19 	Mixed Tundra 
20 	Bare Ground Tundra")

# Stack the landcover designations and the area of each cell into a 2-band raster
s <- stack(landcover, raster::area(landcover))
names(s) <- c("index", "pixel_area")

# make sure the trend raster and the landcover raster line up
r <- raster::projectRaster(from = r, to = s)

# Stack the trend raster on top of the 2-band landcover/pixel area raster
trend <- raster::stack(s, r)

# Get the total area of each landcover by summing the areas of the individual
# pixels
# Convert to a data frame
landcover_area <- 
  as.data.frame(s, xy = TRUE) %>% 
  dplyr::group_by(index) %>% 
  dplyr::summarize(total_area = sum(pixel_area))

# Subset to only landcovers we care about and only where trend >= 14 flammable
# nights
trend_df <- 
  as.data.frame(trend, xy = TRUE) %>% 
  dplyr::filter(index %in% landcovers_of_interest) %>% 
  dplyr::filter(trend >= 14) %>% 
  dplyr::as_tibble()

# Sum the pixel values for the subsetted data frame by landcover type
# Join back to the data frame representing the total area of each landcover
# calculate the % of area experiencing large increases in flammable nights
# Join to data frame representing character labels of landcover types
landcover_trends <-
  trend_df %>% 
  dplyr::group_by(index) %>% 
  dplyr::summarize(gt14_area = sum(pixel_area)) %>% 
  dplyr::left_join(landcover_area, by = "index") %>%
  dplyr::mutate(pct_big_trend = gt14_area / total_area) %>% 
  dplyr::left_join(lc_lookup)

# Sum all high trend pixels per landcover
# Sum all total area per landcover
# calculate overall percent of burnable land experiencing high trends of 
# increases in flammable nights
overall_trend <-
  trend_df %>% 
  dplyr::group_by(index) %>% 
  dplyr::summarize(gt14_area = sum(pixel_area)) %>% 
  dplyr::left_join(landcover_area, by = "index") %>%
  dplyr::summarize(gt14_area = sum(gt14_area), total_area = sum(total_area)) %>% 
  dplyr::mutate(pct_big_trend = gt14_area / total_area)

# Make a pretty table
overall_trend$landcover <- "All burnable landcovers"
overall_trend$index <- NA

overall_trend <- 
  overall_trend %>% 
  dplyr::select(index, everything())

final_table <- 
  rbind(landcover_trends, overall_trend) %>% 
  dplyr::mutate(pct_big_trend = round(pct_big_trend * 100, 1)) %>% 
  setNames(c("Index", ">=14 flammable nights area (km^2)", "Total area (km^2)", "Percent >=14 flammable nights (%)", "Landcover")) %>% 
  gt()

email_table <- gt::as_raw_html(final_table)
email <- blastula::compose_email(body = md(email_table))
email
