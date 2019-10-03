# Purpose: merge the combined global MCD14ML/GLDAS2.1/RESOLVE ecoregion dataset with the global FIRED database.
# We want to have each active fire detection joined with what FIRED event it fell within the spatiotemporal window of (if any).

# Try to build this such that it can run on AWS.

# Load packages
library(tidyverse)
library(sf)
library(data.table)
library(lubridate)
library(aws.s3)
library(doParallel)
library(foreach)

download <- FALSE

# Download the western hemisphere FIRED data from S3 if it doesn't exist on disk
if(!file.exists(file.path("data", "data_raw", "FIRED", "western_hemisphere_to_may2019.gpkg")) & download) {
  dir.create(path = file.path("data", "data_raw", "FIRED"), showWarnings = FALSE, recursive = TRUE)
  fired_west_path <- save_object(object = "western_hemisphere_to_may2019.gpkg",
                                 bucket = "earthlab-natem/modis-burned-area/delineated_events", 
                                 file = file.path("data", "data_raw", "FIRED", "western_hemisphere_to_may2019.gpkg"))
} 

# Download the western hemisphere FIRED data from S3 if it doesn't exist on disk
if(!file.exists(file.path("data", "data_raw", "FIRED", "western_hemisphere_to_may2019.gpkg")) & !download) {
  fired_west <- s3read_using(FUN = sf::st_read, 
                             object = "western_hemisphere_to_may2019.gpkg",
                             bucket = "earthlab-natem/modis-burned-area/delineated_events")
} 

# Read the western hemisphere data into memory as an sf object if it exists on disk
if(file.exists(file.path("data", "data_raw", "FIRED", "western_hemisphere_to_may2019.gpkg"))) {
  fired_west <- sf::st_read(file.path("data", "data_raw", "FIRED", "western_hemisphere_to_may2019.gpkg"))
}

# For now, add a "duration" column so we can filter out rows that have start/last dates that are incompatible
fired_west <- 
  fired_west %>% 
  dplyr::mutate(duration = as.numeric(last_date - start_date))

# For now, filter out the records with a duration < 0
fired <-
  fired_west %>%
  dplyr::filter(duration >= 0)


# the CRS of the fired dataset, so we don't need to keep calling the st_crs function
fired_crs <- st_crs(fired_west)

# Generate daily time steps of burning days for each FIRED event
# This process allows us to use a vectorized operation to filter all the active 
# fire detections into just the ones that occurred on a single day, within a single
# FIRED event

# The function will take in a start and last date for a FIRED event and return a
# tibble where each row represents a daily time step between those two dates
# The resulting tibble has 3 columns representing the year, month, and day that
# the particular FIRED event was burning
# It returns a list so that the function can be run in a vectorized fashion and
# append (using mutate() a list-column to the FIRED dataset
# Unnesting this column then produces 
determine_burning_days <- 
  function(start_date, last_date) {
    burning_days_df <- 
      format(seq(start_date,
                 last_date,
                 by = "day"), 
             format = "%Y-%m-%d") %>% 
      tibble() %>% 
      separate(col = ".", into = c("year", "month", "day"), sep = "-") %>% 
      dplyr::mutate(year = as.numeric(year),
                    month = as.numeric(month),
                    day = as.numeric(day))
    
    return(list(burning_days_df))
  }

# Vectorize the function so that it can work across the whole FIREd dataset at once
v_determine_burning_days <- 
  Vectorize(determine_burning_days, vectorize.args = c("start_date", "last_date"))

# Iterating through all the years of data using a named vector
mcd14ml_years <- 2000:2019
mcd14ml_years_named <- mcd14ml_years %>% setNames(mcd14ml_years)


(start <- Sys.time())

cl <- makeCluster(20)
registerDoParallel(cl)

# Use a foreach() loop so we can easily parallelize it
# include the necessary packages to be run inside the loop using the .packages= argument
# the .final= argument specifies a function to be called on the resulting list after the
# loop runs. In our case, we want to name each list element so that we can fix the 2019
# element more readily
afd_list <- 
  foreach(i = mcd14ml_years_named, .packages = c("tidyverse", "data.table", "sf"), 
          .final = function(x) setNames(x, names(mcd14ml_years_named))) %dopar% {
            
            # Download the mcd14ml/gldas2.1/resolve-ecoregion data for the current year
            if(!file.exists(file.path("data", "data_output", "mcd14ml_gldas21_resolve-ecoregions", paste0("mcd14ml_gldas21_resolve-ecoregions_", i, ".csv"))) & download) {
              dir.create(file.path("data", "data_output", "mcd14ml_gldas21_resolve-ecoregions"), recursive = TRUE)
              download.file(url = paste0("https://earthlab-mkoontz.s3-us-west-2.amazonaws.com/mcd14ml_gldas21_resolve-ecoregions/mcd14ml_gldas21_resolve-ecoregions_", i, ".csv"),
                            destfile = file.path("data", "data_output", "mcd14ml_gldas21_resolve-ecoregions", paste0("mcd14ml_gldas21_resolve-ecoregions_", i, ".csv")))
            } 
            
            if(file.exists(file.path("data", "data_output", "mcd14ml_gldas21_resolve-ecoregions", paste0("mcd14ml_gldas21_resolve-ecoregions_", i, ".csv")))) {
              # Read in the active fire data for this year
              this_afd <- 
                data.table::fread(file.path("data", "data_output", "mcd14ml_gldas21_resolve-ecoregions", paste0("mcd14ml_gldas21_resolve-ecoregions_", i, ".csv"))) 
            }              
            
            # If we don't want to download the data, read it directly into R from S3 bucket
            if(!file.exists(file.path("data", "data_output", "mcd14ml_gldas21_resolve-ecoregions", paste0("mcd14ml_gldas21_resolve-ecoregions_", i, ".csv"))) & !download) {
              this_afd <- s3read_using(FUN = data.table::fread, 
                                       object = paste0("mcd14ml_gldas21_resolve-ecoregions_", i, ".csv"),
                                       bucket = "earthlab-mkoontz/mcd14ml_gldas21_resolve-ecoregions")
            }
            
            # expand the FIRED dataset for the particular year such that each row represents a single
            # burning day for each FIRED event
            fired_by_day <-
              fired %>%
              dplyr::filter(lubridate::year(start_date) == i) %>% 
              dplyr::mutate(burning_days = v_determine_burning_days(start_date, last_date)) %>% 
              tidyr::unnest(cols = burning_days)
            
            # make the VERSION column a character type
            this_afd[, VERSION := as.character(VERSION)]
            
            # Split the active fire detection data by month and day (it already represents a single year)
            this_afd_by_monthday_list <- split(x = this_afd, by = c("acq_month", "acq_day"), sorted = TRUE)
            
            # Iterate through each day of the year and append the active fire detections occurring on that
            # specific day to each row of the FIRED dataset for events occurring on that day
            # The code will also spatially intersect the active fire detections in case there are multiple
            # FIRED events occurring on a single day.
            
            spatiotemporal_match_fired_mcd14ml_list <-
              lapply(names(this_afd_by_monthday_list), FUN = function(x) {
                # These represent the specific  month/day currently being iterated on
                this_afd_name <- strsplit(x, split = "\\.")[[1]]
                this_afd_year <- as.numeric(i)
                this_afd_month <- as.numeric(this_afd_name[1])
                this_afd_day <- as.numeric(this_afd_name[2])
                
                # This object represents the data.table of active fire detections occurring on
                # the current month/day being iterated on
                this_afd_monthday <- this_afd_by_monthday_list[[x]]
                
                # Convert the data.table of active fire detections for this particular day to a
                # spatial object and transform it to the same CRS as the FIRED dataset
                this_afd_sf <- 
                  this_afd_monthday %>% 
                  sf::st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326, remove = FALSE) %>%
                  sf::st_transform(fired_crs)
                
                # filter the FIRED dataset to just the rows representing FIRED events burning on
                # the current day being iterated over (could be multiple events, but always only
                # a single day at a time)
                # spatially intersect the active fire detections with each FIRED event perimeter
                # This will also append the ID and other FIRED attributes to each active fire detection
                # Drop the geometry and convert to a data.table
                this_spatiotemporal_match_fired_mcd14ml <-
                  fired_by_day %>% 
                  dplyr::filter(year == this_afd_year & month == this_afd_month & day == this_afd_day) %>% 
                  sf::st_intersection(this_afd_sf) %>% 
                  sf::st_drop_geometry() %>%
                  data.table::as.data.table()
                
                print(paste(this_afd_year, this_afd_month, this_afd_day, sep = "-"))
                
                # Return the data.table of all fired events with the active fire detections that fall on the
                # same day and within the perimeter of the FIRED event
                return(this_spatiotemporal_match_fired_mcd14ml)
                
              })
            
            # Join the list of spatiotemporally joined FIRED events/active fire detections for the year into a 
            # single data.table
            spatiotemporal_match_fired_mcd14ml <-
              data.table::rbindlist(spatiotemporal_match_fired_mcd14ml_list)
            
            return(spatiotemporal_match_fired_mcd14ml)
          }
stopCluster(cl)
print(Sys.time() - start)

# Add the TYPE column for the Near Real Time product
afd_list[["2019"]]$TYPE <- NA_integer_
afd_list[["2019"]] <- afd_list[["2019"]] %>% dplyr::select(names(afd_list[["2018"]]))

afd <- data.table::rbindlist(afd_list)
print(Sys.time() - start)

