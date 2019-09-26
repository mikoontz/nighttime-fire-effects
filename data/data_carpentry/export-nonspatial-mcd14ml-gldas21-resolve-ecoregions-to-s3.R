# Purpose: export a non-spatial version of the mcd14ml_gldas21_resolve-ecoregion dataset to AWS S3

library(tidyverse)
library(sf)
library(data.table)
library(aws.s3)

paths <- 
  tibble(full_path = grep(list.files(file.path("data", "data_output", "mcd14ml_gldas21_resolve-ecoregions"), full.names = TRUE), pattern = ".csv", inv = TRUE, value = TRUE),
         filename = grep(list.files(file.path("data", "data_output", "mcd14ml_gldas21_resolve-ecoregions")), pattern = ".csv", inv = TRUE, value = TRUE))

afd_list <- mapply(FUN = 
                     function(full_path, filename) {
                       
                       this_afd <- st_read(full_path, stringsAsFactors = FALSE)
                       
                       DT <- st_drop_geometry(this_afd)
                       
                       data.table::fwrite(x = DT, file = paste0(full_path, ".csv"))
                       
                       s3write_using(DT, 
                                     FUN = data.table::fwrite, 
                                     object = paste0(filename, ".csv"), 
                                     bucket = "earthlab-mkoontz/mcd14ml_gldas21_resolve-ecoregions")
                       
                       return(this_afd)
                       
                     }, 
                   full_path = paths$full_path, filename = paths$filename)

afd <- do.call("rbind", afd)