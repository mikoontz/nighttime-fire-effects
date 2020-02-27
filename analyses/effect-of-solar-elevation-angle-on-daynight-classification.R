library(tidyverse)
library(sf)
library(data.table)

# How much does it matter whether we use the center of the sun's disc to calculate day/night
# (basing on solar elevation angle) vs. using the top of the sun's disc?

afd <- lapply(list.files("data/data_output/mcd14ml_solar-elevation-angle/", full.names = TRUE), fread)
afd <- afd[1:19]
afd <- data.table::rbindlist(afd)

afd[, daynight_mid_disc := ifelse(solar_elev_ang > 0, yes = "day", no = "night")]
afd[, daynight_top_disc := ifelse(solar_elev_ang > -(0.53/2), yes = "day", no = "night")]

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