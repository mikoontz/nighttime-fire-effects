# define the day and nighttime fire season by landcover type


library(tidyverse)
library(sf)
library(raster)
library(data.table)
library(cowplot)
library(ncdf4)
library(rasterDT)
library(slider)
# remotes::install_github("wilkelab/ggtext")
library(ggtext)

# get a raster template ---------------------------------------------------
raster_template <- raster::raster("data/data_raw/grid_0_25_degree_vars_modis_D_AFC_num_April_2001.tif")
raster_template <- raster::shift(x = raster_template, dx = -0.125, dy = 0.125)
# get overpass correction data --------------------------------------------

if(!dir.exists("data/data_output/MODIS-overpass-counts_0.25_analysis-ready/month_2003-2018/")) {
  dir.create("data/data_output/MODIS-overpass-counts_0.25_analysis-ready/month_2003-2018/", recursive = TRUE)
}

months <- str_pad(string = as.character(1:12), width = 2, side = "left", pad = "0")

all_rasters <-
  tidyr::crossing(months, daynight = c("day", "night")) %>% 
  dplyr::mutate(s3_path = paste0("https://earthlab-mkoontz.s3-us-west-2.amazonaws.com/MODIS-overpass-counts_0.25_analysis-ready/month_2003-2018/", months, "_2003-2018_", daynight, "_overpass-count.tif"),
                local_path = paste0("data/data_output/MODIS-overpass-counts_0.25_analysis-ready/month_2003-2018/", months, "_2003-2018_", daynight, "_overpass-count.tif"))

local_files <- list.files("data/data_output/MODIS-overpass-counts_0.25_analysis-ready/month_2003-2018/", full.names = TRUE)

to_download <-
  all_rasters %>% 
  dplyr::filter(!(local_path %in% local_files))

purrr::map2(.x = to_download$s3_path, .y = to_download$local_path, .f = function(x, y) {download.file(url = x, destfile = y, method = "curl")})

dir.create("data/data_output/MODIS-overpass-counts_0.25_analysis-ready/month_2003-2018_DT/")

if (length(list.files("data/data_output/MODIS-overpass-counts_0.25_analysis-ready/month_2003-2018_DT/", full.names = TRUE)) == 0) {
all_overpasses <-
  all_rasters %>% 
  purrr::pmap(.f = function(months, daynight, s3_path, local_path) {
    r <- raster::raster(local_path)
    r <- raster::shift(x = r, dx = -0.125, dy = 0.125)
    r_df <- as.data.frame(r, xy = TRUE)
    DT <- as.data.table(r_df) %>% setNames(c("lon", "lat", "overpass_count"))
    DT[, `:=`(daynight = daynight,
              month = months)]
    
    dt_path <- paste0("data/data_output/MODIS-overpass-counts_0.25_analysis-ready/month_2003-2018_DT/", months, "_2003-2018_", daynight, "_overpass-count_DT.csv")
    data.table::fwrite(x = DT, file = dt_path)
    print(dt_path)
    return(DT)
  })
}

all_overpasses <-
  list.files("data/data_output/MODIS-overpass-counts_0.25_analysis-ready/month_2003-2018_DT/", full.names = TRUE) %>% 
  lapply(fread)

all_overpasses <- data.table::rbindlist(all_overpasses)
all_overpasses[, month := str_pad(string = month, width = 2, side = "left", pad = "0")]
data.table::setkeyv(all_overpasses, c("daynight", "month", "lon", "lat"))


# table of the first of each month in DOY ---------------------------------

first_of_months <-
  tibble(month = 1:12, name = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"), name_abbrv = substr(name, start = 1, stop = 3), days_in_month = c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31),
         doy_first = lag(cumsum(days_in_month) + 1)) %>% 
  dplyr::mutate(doy_first = ifelse(month == 1, yes = 1, no = doy_first),
                doy_last = lead(doy_first) - 1,
                doy_last = ifelse(month == 12, yes = 365, no = doy_last),
                doys_in_month = map2(doy_first, doy_last, seq)) %>% 
  dplyr::mutate(doy_first_leap = ifelse(month > 2, yes = doy_first + 1, no = doy_first),
                doy_last_leap = ifelse(month > 1, yes = doy_last + 1, no = doy_last),
                doys_in_month_leap = map2(doy_first_leap, doy_last_leap, seq))

# get MCD14ML active fire detections --------------------------------------

years <- 2003:2018

afd <- lapply(X = years, 
              FUN = function(this_year) {
                fread(paste0("data/data_output/mcd14ml_solar-elevation-angle/mcd14ml_solar-elevation-angle_", this_year, ".csv"))
              })

afd <- data.table::rbindlist(afd)

afd[, `:=`(lat_round = round((LATITUDE + 0.125) * 4) / 4 - 0.125,
           lon_round = round((LONGITUDE + 0.125) * 4) / 4 - 0.125)]
afd[, daynight := ifelse(solar_elev_ang < 0, "night", "day")]
afd[, night := as.numeric(solar_elev_ang < 0)]
afd <- afd[TYPE == 0 & CONFIDENCE >= 10]

afd[, month := str_pad(string = acq_month, width = 2, side = "left", pad = "0")]

# Aggregate by doy and year first, so we can deal with leap years

night_fires_by_lon_lat_year <- afd[, .(N = .N, frp = sum(FRP)), by = .(lon = lon_round, lat = lat_round, year = acq_year, local_doy, daynight)]

night_fires_by_lon_lat_year[local_doy %in% first_of_months$doys_in_month[[1]], month := "01"]
night_fires_by_lon_lat_year[local_doy %in% first_of_months$doys_in_month[[2]], month := "02"]
night_fires_by_lon_lat_year[local_doy %in% first_of_months$doys_in_month[[3]], month := "03"]
night_fires_by_lon_lat_year[local_doy %in% first_of_months$doys_in_month[[4]], month := "04"]
night_fires_by_lon_lat_year[local_doy %in% first_of_months$doys_in_month[[5]], month := "05"]
night_fires_by_lon_lat_year[local_doy %in% first_of_months$doys_in_month[[6]], month := "06"]
night_fires_by_lon_lat_year[local_doy %in% first_of_months$doys_in_month[[7]], month := "07"]
night_fires_by_lon_lat_year[local_doy %in% first_of_months$doys_in_month[[8]], month := "08"]
night_fires_by_lon_lat_year[local_doy %in% first_of_months$doys_in_month[[9]], month := "09"]
night_fires_by_lon_lat_year[local_doy %in% first_of_months$doys_in_month[[10]], month := "10"]
night_fires_by_lon_lat_year[local_doy %in% first_of_months$doys_in_month[[11]], month := "11"]
night_fires_by_lon_lat_year[local_doy %in% first_of_months$doys_in_month[[12]], month := "12"]

night_fires_by_lon_lat_year[local_doy %in% first_of_months$doys_in_month_leap[[1]] & year %in% c(2004, 2008, 2012, 2016), month := "01"]
night_fires_by_lon_lat_year[local_doy %in% first_of_months$doys_in_month_leap[[2]] & year %in% c(2004, 2008, 2012, 2016), month := "02"]
night_fires_by_lon_lat_year[local_doy %in% first_of_months$doys_in_month_leap[[3]] & year %in% c(2004, 2008, 2012, 2016), month := "03"]
night_fires_by_lon_lat_year[local_doy %in% first_of_months$doys_in_month_leap[[4]] & year %in% c(2004, 2008, 2012, 2016), month := "04"]
night_fires_by_lon_lat_year[local_doy %in% first_of_months$doys_in_month_leap[[5]] & year %in% c(2004, 2008, 2012, 2016), month := "05"]
night_fires_by_lon_lat_year[local_doy %in% first_of_months$doys_in_month_leap[[6]] & year %in% c(2004, 2008, 2012, 2016), month := "06"]
night_fires_by_lon_lat_year[local_doy %in% first_of_months$doys_in_month_leap[[7]] & year %in% c(2004, 2008, 2012, 2016), month := "07"]
night_fires_by_lon_lat_year[local_doy %in% first_of_months$doys_in_month_leap[[8]] & year %in% c(2004, 2008, 2012, 2016), month := "08"]
night_fires_by_lon_lat_year[local_doy %in% first_of_months$doys_in_month_leap[[9]] & year %in% c(2004, 2008, 2012, 2016), month := "09"]
night_fires_by_lon_lat_year[local_doy %in% first_of_months$doys_in_month_leap[[10]] & year %in% c(2004, 2008, 2012, 2016), month := "10"]
night_fires_by_lon_lat_year[local_doy %in% first_of_months$doys_in_month_leap[[11]] & year %in% c(2004, 2008, 2012, 2016), month := "11"]
night_fires_by_lon_lat_year[local_doy %in% first_of_months$doys_in_month_leap[[12]] & year %in% c(2004, 2008, 2012, 2016), month := "12"]

data.table::setkeyv(night_fires_by_lon_lat_year, cols = c("daynight", "month", "lon", "lat"))
detections_per_doy_lon_lat_year <- all_overpasses[night_fires_by_lon_lat_year]

# overpasses_per_DOY_2003_2018 represents the cumulative number of overpasses for that DOY/lon/lat combination
# across all 16 years -- 2003 to 2018

detections_per_doy_lon_lat_year[month %in% c("01", "03", "05", "07", "08", "10", "12"), overpasses_per_DOY_2003_2018 := overpass_count / 31]
detections_per_doy_lon_lat_year[month %in% c("04", "06", "09", "11"), overpasses_per_DOY_2003_2018 := overpass_count / 30]
detections_per_doy_lon_lat_year[(month == "02") & (year %in% c(2004, 2008, 2012, 2016)), overpasses_per_DOY_2003_2018 := overpass_count / 29]
detections_per_doy_lon_lat_year[(month == "02") & (!(year %in% c(2004, 2008, 2012, 2016))), overpasses_per_DOY_2003_2018 := overpass_count / 28]

data.table::setkeyv(detections_per_doy_lon_lat_year, cols = c("lon", "lat"))

# get landcover data ------------------------------------------------------

landcovers_of_interest <- c(1:10, 12)

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

total_landcover_areas <-
  landcover_DT %>% 
  dplyr::group_by(index) %>% 
  dplyr::summarize(total_area_of_landcover_km2 = as.numeric(sum(pixel_area_km2, na.rm = TRUE))) %>% 
  dplyr::left_join(landcover_table, by = "index")

total_landcover_areas_hemisphere <-
  landcover_DT %>% 
  dplyr::mutate(hemisphere = ifelse(lat < 0, yes = "southern", no = "northern")) %>% 
  dplyr::group_by(index, hemisphere) %>% 
  dplyr::summarize(total_area_of_landcover_km2 = as.numeric(sum(pixel_area_km2, na.rm = TRUE))) %>% 
  dplyr::left_join(landcover_table, by = "index")

detections_per_doy_lon_lat_year_landcover <- landcover_DT[detections_per_doy_lon_lat_year]
data.table::setkeyv(detections_per_doy_lon_lat_year_landcover, cols = c("lon", "lat", "index"))

detections_per_doy_year_landcover <- detections_per_doy_lon_lat_year_landcover[, .(area_with_detections = sum(pixel_area_km2)), by = .(index, daynight, year, local_doy)]

detections_per_doy_year_landcover <-
  detections_per_doy_year_landcover %>% 
  dplyr::left_join(landcover_table) %>% 
  as.data.table()

area_with_detections_per_doy_landcover <- detections_per_doy_year_landcover[, .(mean_area_with_detections_per_year = mean(area_with_detections)), by = .(index, daynight, local_doy)]

area_with_detections_per_doy_landcover <- 
  area_with_detections_per_doy_landcover %>% 
  dplyr::left_join(total_landcover_areas, by = "index") %>% 
  as.data.table()

# Here, the mean of the overpasses_per_DOY_2003_2018 variable is taken across the stack of 16 years
# to account for leap year shifts in which DOY corresponds to which calendar day. For instance, in 
# 2003, the 29th day of the year was in March (March 1st), but in 2004-- a leap year, the 29th day was
# in February. We tried to account for this by carefully dividing up the number of overpasses per
# lon/lat per month into categorical months and dividing by the number of days in that month (given 
# whether or not it was a leap year)
# In any case, taking the mean of overpasses_per_DOY_2003_2018 won't likely change these values
# dramatically, but it helps get our data.frame into the right shape for joining and summarizing.
detections_per_doy_lon_lat <- 
  detections_per_doy_lon_lat_year[, .(total_detections = sum(N),
                                      overpasses_per_DOY_2003_2018 = mean(overpasses_per_DOY_2003_2018),
                                      n_years_the_pixel_burned = .N,
                                      total_frp = sum(frp)),
                                  by = .(lon, lat, local_doy, daynight)]

# mean_detections_per_overpass represents the total number of detections per lon/lat across
# the 16-year period divided by the total number of overpasses per lon/lat across the 16 year period
# Thus we can think of it as the expected number of detections per year per overpass
detections_per_doy_lon_lat[, `:=`(mean_detections_per_overpass = total_detections / overpasses_per_DOY_2003_2018,
                                  frp_per_overpass = total_frp / overpasses_per_DOY_2003_2018)]
detections_per_doy_lon_lat[, hemisphere := ifelse(lat < 0, yes = "southern", no = "northern")]

detections_per_doy_lon_lat_landcover <- landcover_DT[detections_per_doy_lon_lat]

# Here, we sum across all the different lon/lat values into groups by landcover instead
# total_detections is all of the detections across 16 years in that landcover type
# mean_detetions_per_overpass_on_DOY is the expected annual detection/overpass across the whole
# landcover type
detections_per_doy_landcover <-  
  detections_per_doy_lon_lat_landcover[!is.infinite(mean_detections_per_overpass), 
                                       .(total_detections = sum(total_detections),
                                         total_frp = sum(total_frp),
                                         mean_detections_per_overpass_on_DOY = sum(mean_detections_per_overpass),
                                         frp_per_overpass_on_DOY = sum(frp_per_overpass),
                                         sum_area_affected_by_detections_on_DOY_km2 = sum(pixel_area_km2),
                                         mean_annual_area_affected_by_detections_on_DOY_km2 = sum(pixel_area_km2 * n_years_the_pixel_burned / 16),
                                         n_pixels_with_detections = .N), 
                                       by = .(index, local_doy, daynight)]

# Join with the table representing how much land area is covered by each landcover type
detections_per_doy_landcover <-
  detections_per_doy_landcover %>% 
  dplyr::left_join(total_landcover_areas, by = "index") %>% 
  dplyr::mutate(mean_detections_per_overpass_on_DOY_per_km2 = 1000 * mean_detections_per_overpass_on_DOY / total_area_of_landcover_km2,
                pct_of_landcover_area_affected_by_detections = mean_annual_area_affected_by_detections_on_DOY_km2 / total_area_of_landcover_km2,
                detections_per_pixel = total_detections / n_pixels_with_detections,
                frp_per_detection = total_frp / total_detections,
                frp_per_overpass_on_DOY_per_km2 = frp_per_overpass_on_DOY / total_area_of_landcover_km2) %>% 
  as.data.table()

# Same set of aggregations as above, except now also breaking down by northern and southern
# hemisphere
detections_per_doy_landcover_hemisphere <-  
  detections_per_doy_lon_lat_landcover[!is.infinite(mean_detections_per_overpass), 
                                       .(total_detections = sum(total_detections),
                                         total_frp = sum(total_frp),
                                         mean_detections_per_overpass_on_DOY = sum(mean_detections_per_overpass),
                                         frp_per_overpass_on_DOY = sum(frp_per_overpass),
                                         sum_area_affected_by_detections_on_DOY_km2 = sum(pixel_area_km2),
                                         mean_annual_area_affected_by_detections_on_DOY_km2 = sum(pixel_area_km2 * n_years_the_pixel_burned / 16),
                                         n_pixels_with_detections = .N), 
                                       by = .(index, hemisphere, local_doy, daynight)]

detections_per_doy_landcover_hemisphere <-
  detections_per_doy_landcover_hemisphere %>% 
  dplyr::left_join(total_landcover_areas_hemisphere, by = c("index", "hemisphere")) %>% 
  dplyr::mutate(mean_detections_per_overpass_on_DOY_per_km2 = 1000 * mean_detections_per_overpass_on_DOY / total_area_of_landcover_km2,
                pct_of_landcover_area_affected_by_detections = mean_annual_area_affected_by_detections_on_DOY_km2 / total_area_of_landcover_km2,
                detections_per_pixel = total_detections / n_pixels_with_detections,
                frp_per_detection = total_frp / total_detections,
                frp_per_overpass_on_DOY_per_km2 = frp_per_overpass_on_DOY / total_area_of_landcover_km2) %>% 
  as.data.table()


# defining the seasons ----------------------------------------------------

doy_slider <- function(doy, this_afd, delta) {
  
  start_doy <- doy - 1 - delta
  end_doy <- doy + delta
  
  doy_seq <- start_doy:end_doy
  
  doy_seq <- ifelse(doy_seq < 1, yes = doy_seq + 365, no = doy_seq)
  doy_seq <- ifelse(doy_seq > 365, yes = doy_seq - 365, no = doy_seq)
  
  smoothed_data <- mean(this_afd$mean_detections_per_overpass_on_DOY_per_km2[this_afd$local_doy %in% doy_seq])
  return(smoothed_data)
}


doy_slider_frp <- function(doy, this_afd, delta) {
  
  start_doy <- doy - 1 - delta
  end_doy <- doy + delta
  
  doy_seq <- start_doy:end_doy
  
  doy_seq <- ifelse(doy_seq < 1, yes = doy_seq + 365, no = doy_seq)
  doy_seq <- ifelse(doy_seq > 365, yes = doy_seq - 365, no = doy_seq)
  
  smoothed_data <- mean(this_afd$frp_per_overpass_on_DOY_per_km2[this_afd$local_doy %in% doy_seq])
  return(smoothed_data)
}


# Correct for the total landcover
alpha_low <- 0.25
alpha_high <- 1

afd_of_interest <- 
  detections_per_doy_landcover %>% 
  dplyr::filter(index %in% landcovers_of_interest) %>% 
  dplyr::filter(local_doy != 366) %>% 
  dplyr::arrange(index, daynight, local_doy) %>% 
  dplyr::group_by(index, daynight) %>% 
  dplyr::group_modify(.f = function(.x, ...) {
    
    .x <-
      .x %>% 
      dplyr::mutate(smoothed_detections = purrr::map_dbl(local_doy, 
                                                         .f = doy_slider,
                                                         this_afd = .,
                                                         delta = 15),
                    smoothed_frp = purrr::map_dbl(local_doy,
                                                  .f = doy_slider_frp,
                                                  this_afd = .,
                                                  delta = 15)) %>% 
      dplyr::mutate(over_threshold_raw = ifelse(mean_detections_per_overpass_on_DOY_per_km2 < (12/3650) * sum(mean_detections_per_overpass_on_DOY_per_km2), 
                                                yes = alpha_low,
                                                no = alpha_high),
                    over_threshold_smooth = ifelse(smoothed_detections < (12/3650) * sum(smoothed_detections),
                                                   yes = alpha_low,
                                                   no = alpha_high),
                    over_threshold_smooth_frp = ifelse(smoothed_frp < (12/3650) * sum(smoothed_frp),
                                                   yes = alpha_low,
                                                   no = alpha_high)) %>% 
      dplyr::mutate(thresholded_detections_raw = ifelse(over_threshold_raw == alpha_high,
                                                        yes = mean_detections_per_overpass_on_DOY_per_km2,
                                                        no = NA),
                    thresholded_detections_smooth = ifelse(over_threshold_smooth == alpha_high,
                                                           yes = smoothed_detections,
                                                           no = NA))
    return(.x)
  })

# Determine landcover order
landcover_order <- 
  afd_of_interest %>% 
  dplyr::filter(daynight == "night") %>% 
  dplyr::summarize(detections = sum(smoothed_detections)) %>% 
  dplyr::ungroup() %>% 
  dplyr::left_join(landcover_table, by = "index") %>% 
  dplyr::mutate(landcover_split = factor(landcover_split, levels = landcover_split[order(detections, decreasing = TRUE)]))

afd_of_interest <-
  afd_of_interest %>% 
  dplyr::mutate(landcover_split = factor(landcover_split, levels = levels(landcover_order$landcover_split)))

# Plot
gg_fire_season <-
  ggplot(afd_of_interest, aes(x = local_doy, y = 1000 * smoothed_detections, color = daynight, alpha = over_threshold_smooth)) +
  geom_line(lwd = 1) +
  theme_bw(base_size = 10) +
  theme(strip.text = element_text(angle = 0, face = "bold"),
        strip.background = element_rect(fill = "white"),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "pt")),
        axis.text = element_text(),
        axis.title.x = element_blank(),
        legend.position = c(5/6, 1/8)) +
  scale_color_manual(values = c("red", "black")) +
  facet_wrap(facets = vars(landcover_split), ncol = 3) +
  labs(x = "Day of year",
       y = bquote("Mean detections per day per overpass per 1,000" ~ km^2),
       color = "Day or night?") +
  guides(alpha = FALSE) +
  scale_x_continuous(breaks = first_of_months$doy_first, labels = first_of_months$name_abbrv) +
  scale_y_log10(labels = scales::comma) +
  geom_vline(xintercept = first_of_months$doy_first) +
  coord_polar() +
  scale_alpha_identity()

gg_fire_season

ggsave(filename = "figures/fire-seasonality_daynight-landcover.png", plot = gg_fire_season, width = 183, height = 4/3 * 183, units = "mm")


# FRP

gg_fire_season_frp <-
  ggplot(afd_of_interest, aes(x = local_doy, y = 1000 * smoothed_frp, color = daynight, alpha = over_threshold_smooth_frp)) +
  geom_line(lwd = 1) +
  theme_bw(base_size = 10) +
  theme(strip.text = element_text(angle = 0, face = "bold"),
        strip.background = element_rect(fill = "white"),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "pt")),
        axis.text = element_text(),
        axis.title.x = element_blank(),
        legend.position = c(5/6, 1/8)) +
  scale_color_manual(values = c("red", "black")) +
  facet_wrap(facets = vars(landcover_split), ncol = 3) +
  labs(x = "Day of year",
       y = bquote("Mean FRP per day per overpass per 1,000" ~ km^2),
       color = "Day or night?") +
  guides(alpha = FALSE) +
  scale_x_continuous(breaks = first_of_months$doy_first, labels = first_of_months$name_abbrv) +
  scale_y_log10(labels = scales::comma) +
  geom_vline(xintercept = first_of_months$doy_first) +
  coord_polar() +
  scale_alpha_identity()

gg_fire_season_frp


ggsave(filename = "figures/fire-seasonality-frp_daynight-landcover.png", plot = gg_fire_season_frp, width = 183, height = 4/3 * 183, units = "mm")




afd_of_interest %>% dplyr::select(landcover_split, local_doy,mean_detections_per_overpass_on_DOY_per_km2, smoothed_detections, daynight, over_threshold_smooth) %>% 
  dplyr::filter(local_doy == 16) %>% 
  slice(1:100) %>% 
  as.data.frame()

# by hemisphere also ------------------------------------------------------

afd_of_interest <- 
  detections_per_doy_landcover_hemisphere %>% 
  dplyr::filter(index %in% landcovers_of_interest) %>% 
  dplyr::filter(local_doy != 366) %>% 
  dplyr::arrange(hemisphere, index, daynight, local_doy) %>% 
  dplyr::group_by(index, daynight, hemisphere) %>% 
  dplyr::group_modify(.f = function(.x, ...) {
    
    .x <-
      .x %>% 
      dplyr::mutate(smoothed_detections = purrr::map_dbl(local_doy, 
                                                         .f = doy_slider,
                                                         afd = .,
                                                         delta = 15)) %>% 
      dplyr::mutate(over_threshold_raw = ifelse(mean_detections_per_overpass_on_DOY_per_km2 < (12/3650) * sum(mean_detections_per_overpass_on_DOY_per_km2), 
                                                yes = alpha_low,
                                                no = alpha_high),
                    over_threshold_smooth = ifelse(smoothed_detections < (12/3650) * sum(smoothed_detections),
                                                   yes = alpha_low,
                                                   no = alpha_high)) %>% 
      dplyr::mutate(thresholded_detections_raw = ifelse(over_threshold_raw == alpha_high,
                                                        yes = mean_detections_per_overpass_on_DOY_per_km2,
                                                        no = NA),
                    thresholded_detections_smooth = ifelse(over_threshold_smooth == alpha_high,
                                                           yes = smoothed_detections,
                                                           no = NA))
    return(.x)
  })

afd_of_interest <-
  afd_of_interest %>% 
  dplyr::mutate(landcover_split = factor(landcover_split, levels = levels(landcover_order$landcover_split)))


# log10 scale

gg_fire_season_northern_hemisphere <- 
  ggplot(afd_of_interest %>% dplyr::filter(hemisphere == "northern"), aes(x = local_doy, y = 1000 * smoothed_detections, color = daynight, alpha = over_threshold_smooth)) +
  geom_line() +
  theme_bw(base_size = 10) +
  theme(strip.text = element_text(angle = 0, face = "bold"),
        strip.background = element_rect(fill = "white"),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "pt")),
        axis.text = element_text(),
        axis.title.x = element_blank(),
        legend.position = c(5/6, 1/8)) +
  scale_color_manual(values = c("red", "black")) +
  facet_wrap(facets = vars(landcover_split), ncol = 3) +
  labs(x = "Day of year",
       y = bquote("Mean detections per day per overpass per 1,000" ~ km^2),
       color = "Day or night?",
       title = "Northern hemisphere") +
  guides(alpha = FALSE) +
  scale_x_continuous(breaks = first_of_months$doy_first, labels = first_of_months$name_abbrv) +
  scale_y_log10(labels = scales::comma) +
  geom_vline(xintercept = first_of_months$doy_first) +
  coord_polar() +
  scale_alpha_identity()

gg_fire_season_northern_hemisphere

ggsave(filename = "figures/fire-seasonality_daynight-landcover-northern-hemisphere.png", plot = gg_fire_season_northern_hemisphere, width = 183, height = 4/3 * 183, units = "mm")


gg_fire_season_southern_hemisphere <- 
  ggplot(afd_of_interest %>% dplyr::filter(hemisphere == "southern"), aes(x = local_doy, y = 1000 * smoothed_detections, color = daynight, alpha = over_threshold_smooth)) +
  geom_line() +
  theme_bw(base_size = 10) +
  theme(strip.text = element_text(angle = 0, face = "bold"),
        strip.background = element_rect(fill = "white"),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "pt")),
        axis.text = element_text(),
        axis.title.x = element_blank(),
        legend.position = c(5/6, 1/8)) +
  scale_color_manual(values = c("red", "black")) +
  facet_wrap(facets = vars(landcover_split), ncol = 3, drop = FALSE) +
  labs(x = "Day of year",
       y = bquote("Mean detections per day per overpass per 1,000" ~ km^2),
       color = "Day or night?",
       title = "Southern hemisphere") +
  guides(alpha = FALSE) +
  scale_x_continuous(breaks = first_of_months$doy_first, labels = first_of_months$name_abbrv) +
  scale_y_log10(labels = scales::comma) +
  geom_vline(xintercept = first_of_months$doy_first) +
  coord_polar() +
  scale_alpha_identity()

gg_fire_season_southern_hemisphere

ggsave(filename = "figures/fire-seasonality_daynight-landcover-southern-hemisphere.png", plot = gg_fire_season_southern_hemisphere, width = 183, height = 4/3 * 183, units = "mm")

### Old figures

# Total detections

# ggplot(afd_of_interest, aes(x = local_doy, y = total_detections, color = daynight)) +
#   geom_line(lwd = 2) +
#   theme_bw() +
#   scale_color_manual(values = c("red", "black")) +
#   facet_grid(rows = vars(landcover), scales = "free_y") +
#   labs(x = "Day of year",
#        y = "Total detections",
#        title = "Total detections per day of year across the 2003-2018 period")
# 
# ggplot(afd_of_interest, aes(x = local_doy, y = total_detections, color = daynight)) +
#   geom_line(lwd = 2) +
#   theme_bw() +
#   scale_color_manual(values = c("red", "black")) +
#   facet_wrap(facets = vars(landcover)) +
#   labs(x = "Day of year",
#        y = "Total detections",
#        title = "Total detections per day of year across the 2003-2018 period") +
#   coord_polar() +
#   scale_y_log10()
# 
# # Detections corrected for overpass frequency and land area of each landcover type
# 
# ggplot(afd_of_interest, aes(x = local_doy, y = mean_detections_per_overpass_on_DOY_per_km2, color = daynight)) +
#   geom_line(lwd = 2) +
#   theme_bw() +
#   scale_color_manual(values = c("red", "black")) +
#   facet_grid(rows = vars(landcover), scales = "free_y") +
#   labs(x = "Day of year",
#        y = "Detections per overpass per 1000 km^2^",
#        title = "Detections per day of year per overpass per 1000 km2")
# 
# ggplot(afd_of_interest, aes(x = local_doy, y = smoothed_detections, color = daynight)) +
#   geom_line(lwd = 2) +
#   theme_bw() +
#   scale_color_manual(values = c("red", "black")) +
#   facet_grid(rows = vars(landcover), scales = "free_y") +
#   labs(x = "Day of year",
#        y = "Detections per overpass per 1000 km^2^",
#        title = "Detections per day of year per overpass per 1000 km2")

# ggplot(afd_of_interest, aes(x = local_doy, y = mean_detections_per_overpass_on_DOY_per_km2, color = daynight)) +
#   geom_line(lwd = 2) +
#   theme_bw() +
#   scale_color_manual(values = c("red", "black")) +
#   facet_wrap(facets = vars(landcover)) +
#   labs(x = "Day of year",
#        y = "Detections per overpass per 1000 km^2^",
#        title = "Detections per day of year per overpass per 1000 km2 (log10 scale)") +
#   scale_y_log10() +
#   geom_vline(xintercept = first_of_months$doy_first) +
#   scale_x_continuous(breaks = first_of_months$doy_first, labels = paste(first_of_months$name, "1")) +
#   coord_polar()


# log scale

# ggplot(afd_of_interest, aes(x = local_doy, y = mean_detections_per_overpass_on_DOY_per_km2, color = daynight)) +
#   geom_line(lwd = 2) +
#   theme_bw() +
#   scale_color_manual(values = c("red", "black")) +
#   facet_grid(rows = vars(landcover), scales = "free_y") +
#   labs(x = "Day of year",
#        y = "Detections per overpass per 1000 km^2^",
#        title = "Detections per day of year per overpass per 1000 km2 (log10 scale)") +
#   scale_y_log10()


# just nighttime corrected detections
# ggplot(afd_of_interest %>% dplyr::filter(daynight == "night"), aes(x = local_doy, y = mean_detections_per_overpass_on_DOY_per_km2, color = daynight)) +
#   geom_line(lwd = 2) +
#   theme_bw() +
#   scale_color_manual(values = c("black")) +
#   facet_grid(rows = vars(landcover), scales = "free_y") +
#   labs(x = "Day of year",
#        y = "Detections per overpass per 1000 km^2^",
#        title = "Detections per day of year per overpass per 1000 km2")

# Percent of land area affected

# ggplot(afd_of_interest, aes(x = local_doy, y = pct_of_landcover_area_affected_by_detections, color = daynight)) +
#   geom_line(lwd = 2) +
#   theme_bw() +
#   scale_color_manual(values = c("red", "black")) +
#   facet_grid(rows = vars(landcover), scales = "free_y") +
#   labs(x = "Day of year",
#        y = "Percent",
#        title = "Mean percent of landcover affected by fire detections per year")
# 
# ggplot(afd_of_interest, aes(x = local_doy, y = pct_of_landcover_area_affected_by_detections, color = daynight)) +
#   geom_line(lwd = 2) +
#   theme_bw() +
#   scale_color_manual(values = c("red", "black")) +
#   facet_wrap(facets = vars(landcover)) +
#   coord_polar() +
#   labs(x = "Day of year",
#        y = "Percent",
#        title = "Mean percent of landcover affected by fire detections per year")
# 
# 
# # Just nighttime percent of land area affected
# ggplot(afd_of_interest %>% dplyr::filter(daynight == "night"), aes(x = local_doy, y = pct_of_landcover_area_affected_by_detections)) +
#   geom_line(lwd = 2) +
#   theme_bw() +
#   facet_grid(rows = vars(landcover), scales = "free_y")


# With thresholds

# ggplot(afd_of_interest, aes(x = local_doy, y = thresholded_detections_smooth, color = daynight)) +
#   geom_line(lwd = 2) +
#   theme_bw(base_size = 14) +
#   theme(strip.text.y = element_text(
#     size = 12, angle = 0
#   )) +
#   scale_color_manual(values = c("red", "black")) +
#   facet_grid(rows = vars(landcover_split), scales = "free_y") +
#   labs(x = "Day of year",
#        y = "Mean detections per day per overpass per km2",
#        title = "Day and nighttime fire season  
#        Mean annual number of MODIS active fire detections per overpass per km^2 (using a 31-day smoothing window)  
#        (value for a day/landcover/daynight combination must be above 1.2 times the daily average following the rule of Giglio et al., 2006)",
#        color = "Day or night?") +
#   theme(plot.title = element_textbox_simple()) +
#   scale_x_continuous(breaks = first_of_months$doy_first, labels = paste(first_of_months$name, "1"))


# labs(x = "Day of year",
#      y = "Mean detections per day per overpass per km2",
#      title = "Day and nighttime fire season  
#        Mean annual number of MODIS active fire detections per overpass per km^2 (using a 31-day smoothing window)  
#        (value for a day/landcover/daynight combination must be above 1.2 times the daily average following the rule of Giglio et al., 2006)",
#      color = "Day or night?")



#log10 scale
# gg_fire_season <-
#   ggplot(afd_of_interest, aes(x = local_doy, y = thresholded_detections_smooth, color = daynight)) +
#   geom_line(lwd = 2) +
#   theme_bw(base_size = 14) +
#   theme(strip.text.y = element_text(
#     size = 12, angle = 0
#   )) +
#   scale_color_manual(values = c("red", "black")) +
#   facet_grid(rows = vars(landcover_split), scales = "free_y") +
#   labs(x = "Day of year",
#        y = "Mean detections per day per overpass per km2",
#        title = "Day and nighttime fire season  
#        Mean annual number of MODIS active fire detections per overpass per km^2 (using a 31-day smoothing window)  
#        (value for a day/landcover/daynight combination must be above 1.2 times the daily average following the rule of Giglio et al., 2006)",
#        color = "Day or night?") +
#   theme(plot.title = element_textbox_simple()) +
#   scale_x_continuous(breaks = first_of_months$doy_first, labels = paste(first_of_months$name_abbrv, "1")) +
#   scale_y_log10()




#  by hemisphere ----------------------------------------------------------

# ggplot(afd_of_interest, aes(x = local_doy, y = thresholded_detections_smooth, color = daynight, lty = hemisphere)) +
#   geom_line(lwd = 1) +
#   theme_bw(base_size = 14) +
#   theme(strip.text.y = element_text(size = 12, angle = 0)) +
#   scale_color_manual(values = c("red", "black")) +
#   facet_grid(rows = vars(landcover_split), scales = "free_y") +
#   labs(x = "Day of year",
#        y = "Mean detections per day per overpass per km2",
#        title = "Day and nighttime fire season  
#        Mean annual number of MODIS active fire detections per overpass per km^2 (using a 31-day smoothing window)  
#        (value for a day/landcover/daynight/hemisphere combination must be above 1.2 times the daily average following the rule of Giglio et al., 2006)",
#        color = "Day or night?",
#        lty = "Hemisphere") +
#   scale_x_continuous(breaks = first_of_months$doy_first, labels = paste(first_of_months$name, "1")) +
#   theme(plot.title = element_textbox_simple())
# 
# gg_fire_season_hemisphere <- 
#   ggplot(afd_of_interest, aes(x = local_doy, y = thresholded_detections_smooth, color = daynight, lty = hemisphere)) +
#   geom_line(lwd = 1) +
#   theme_bw(base_size = 14) +
#   theme(strip.text.y = element_text(size = 12, angle = 0)) +
#   scale_color_manual(values = c("red", "black")) +
#   facet_grid(rows = vars(landcover_split), scales = "free_y") +
#   labs(x = "Day of year",
#        y = "Mean detections per day per overpass per km2",
#        title = "Day and nighttime fire season  
#        Mean annual number of MODIS active fire detections per overpass per km^2 (using a 31-day smoothing window)  
#        (value for a day/landcover/daynight/hemisphere combination must be above 1.2 times the daily average following the rule of Giglio et al., 2006)",
#        color = "Day or night?",
#        lty = "Hemisphere") +
#   scale_x_continuous(breaks = first_of_months$doy_first, labels = paste(first_of_months$name, "1")) +
#   theme(plot.title = element_textbox_simple()) +
#   scale_y_log10()



# gg_fire_season <-
#   ggplot(afd_of_interest, aes(x = local_doy, y = thresholded_detections_smooth, color = daynight)) +
#   geom_line(lwd = 2) +
#   theme_bw(base_size = 14) +
#   theme(strip.text.y = element_text(size = 12, angle = 0),
#         plot.title = element_textbox_simple(),
#         legend.position = c(0.875, 1/6)) +
#   scale_color_manual(values = c("red", "black")) +
#   facet_wrap(facets = vars(landcover_split)) +
#   labs(x = "Day of year",
#        y = "Mean detections per day per overpass per km2",
#        color = "Day or night?") +
#   scale_x_continuous(breaks = first_of_months$doy_first, labels = paste(first_of_months$name_abbrv, "1")) +
#   scale_y_log10() +
#   geom_vline(xintercept = first_of_months$doy_first) +
#   coord_polar()



# gg_fire_season_hemisphere <- 
#   ggplot(afd_of_interest, aes(x = local_doy, y = 1000 * thresholded_detections_smooth, color = daynight, lty = hemisphere)) +
#   geom_line() +
#   theme_bw(base_size = 10) +
#   theme(strip.text = element_text(angle = 0, face = "bold"),
#         strip.background = element_rect(fill = "white"),
#         axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "pt")),
#         axis.text = element_text(),
#         axis.title.x = element_blank(),
#         legend.position = c(5/6, 1/8)) +
#   scale_color_manual(values = c("red", "black")) +
#   facet_wrap(facets = vars(landcover_split), ncol = 3) +
#   labs(x = "Day of year",
#        y = bquote("Mean detections per day per overpass per 1,000" ~ km^2),
#        color = "Day or night?",
#        title = "Northern hemisphere") +
#   guides(alpha = FALSE) +
#   scale_x_continuous(breaks = first_of_months$doy_first, labels = first_of_months$name_abbrv) +
#   scale_y_log10(labels = scales::comma) +
#   geom_vline(xintercept = first_of_months$doy_first) +
#   coord_polar() +
#   scale_alpha_identity()

