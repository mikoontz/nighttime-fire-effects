# Purpose: climate per active fire detection
# Conditional on there being an active fire detection, what is the climate?

library(tidyverse)
library(sf)
library(data.table)
library(tdigest)
library(lubridate)
library(tmap)
library(mgcv)
library(gganimate)
library(viridis)

if(!file.exists("data/data_output/mcd14ml_gldas21/mcd14ml_with_gldas_climate_variables.csv")) {
  source("data/data_carpentry/merge_mcd14ml_with_gldas2.1.R")
}

afd_filtered <- data.table::fread("data/data_output/mcd14ml-with-gldas2.1-climate-variables/mcd14ml_with_gldas_climate_variables.csv")

# Where are there positive solar angles being called nighttime burning?
# About 2.5%
data(World)

weird <- afd_filtered[solar_elev_ang > 0 & DAYNIGHT == "N"]
weird
weird_sf <- st_as_sf(weird, coords = c("LONGITUDE", "LATITUDE"), crs = 4326) %>% st_transform(st_crs(World))

# rm(afd)

pdf("figures/distribution-of-vpd-daynight.pdf")
plot(density(afd_filtered[afd_filtered$DAYNIGHT == "D", "vpd_hPa"][[1]], na.rm = TRUE), col = viridis(2)[2], ylim = c(0, 0.05), main = "Distribution of VPD for day vs. night active fire detections", lwd = 2, xlab = "Vapor Pressure Deficit (hPa)")
lines(density(afd_filtered[afd_filtered$DAYNIGHT == "N", "vpd_hPa"][[1]], na.rm = TRUE), col = viridis(2)[1], lwd = 2)
legend("topright", legend = c("Day", "Night"), col = viridis(2)[2:1], lty = 1, lwd = 3, bty = "n")
dev.off()

pdf("figures/night-fire-activity-with-positive-solar-elevation-angle.pdf", height = 8, width = 13)
plot(World$geometry, 
     main = "'Night' fire activity with a positive solar elevation angle",
     pch = 0.5)
plot(weird_sf$geometry, add = TRUE, pch = 19, cex = 0.5, col = "red")
dev.off()

# Figure out solar elevation angle (angular distance between horizon and sun)
# Need hour angle (angle away from local solar noon)
# Need declination of sun (based on position of earth in its revolution around sun)
# Need latitude
# 

afd_filtered[, months_elapsed := (acq_year - 2001) * 12 + 2 + acq_month]
afd_filtered[, days_elapsed := as.numeric(difftime(time1 = acq_datetime, time2 = ymd("2000-11-01"), units = "days"))]


# months elapsed, daynight, hemisphere -------------------------------

afd_summary_months_elapsed_daynight_hemisphere <-
  afd_filtered[, .(mean_vpd = mean(vpd_hPa, na.rm = TRUE),
                   p10_vpd = tquantile(tdigest(vpd_hPa), probs = 0.1),
                   p90_vpd = tquantile(tdigest(vpd_hPa), probs = 0.9),
                   n_vpd_lt_10 = length(which(vpd_hPa < 10)),
                   n = length(vpd_hPa)),
               .(DAYNIGHT, hemisphere, months_elapsed)]

filler_months_elapsed_daynight_hemisphere <- 
  as.data.frame(expand.grid(acq_year = c(2003, 2006), 
                            hemisphere = c("Northern hemisphere", "Southern hemisphere"), 
                            acq_month = 1:12, 
                            DAYNIGHT = c("D", "N"))) %>% 
  dplyr::mutate(mean_vpd = NA, 
                p10_vpd = NA,
                p90_vpd = NA,
                months_elapsed = (acq_year - 2001) * 12 + 2 + acq_month,
                n_vpd_lt_10 = NA,
                n = NA)

afd_summary_months_elapsed_daynight_hemisphere <- 
  filler_months_elapsed_daynight_hemisphere %>% 
  dplyr::select(DAYNIGHT, hemisphere, months_elapsed, mean_vpd, p10_vpd, p90_vpd, n_vpd_lt_10, n) %>% 
  rbind(afd_summary_months_elapsed_daynight_hemisphere) %>% 
  dplyr::mutate(pct_vpd_lt_10 = n_vpd_lt_10 / n)


ggplot(afd_summary_months_elapsed_daynight_hemisphere %>% filter(months_elapsed > 19), aes(x = months_elapsed, y = mean_vpd, color = DAYNIGHT)) +
  geom_line(lwd = 1.2) +
  geom_ribbon(aes(ymin = p10_vpd, ymax = p90_vpd), alpha = 0.2) +
  theme_bw() +
  labs(x = "Year",
       y = "Vapor pressure deficit (hPa)",
       fill = "Day or Night detection",
       color = "Day or Night detection") +
  scale_fill_viridis_d(option = "E", direction = -1) +
  scale_color_viridis_d(option = "E", direction = -1) +
  # facet_grid(cols = vars(DAYNIGHT)) +
  geom_smooth(lty = 2, se = FALSE) +
  scale_x_continuous(breaks = seq(3, 220, by = 12), labels = 2001:2019) +
  facet_grid(rows = vars(hemisphere))

ggplot(afd_summary_months_elapsed_daynight_hemisphere %>% filter(months_elapsed > 19), aes(x = months_elapsed, y = n, color = DAYNIGHT)) +
  geom_line(lwd = 1.2) +
  theme_bw() +
  labs(x = "Year",
       y = "Number of fires",
       fill = "Day or Night detection",
       color = "Day or Night detection") +
  scale_fill_viridis_d(option = "E", direction = -1) +
  scale_color_viridis_d(option = "E", direction = -1) +
  # facet_grid(cols = vars(DAYNIGHT)) +
  geom_smooth(lty = 2, se = FALSE) +
  scale_x_continuous(breaks = seq(3, 220, by = 12), labels = 2001:2019) +
  facet_grid(rows = vars(hemisphere))

ggplot(afd_summary_months_elapsed_daynight_hemisphere %>% filter(months_elapsed > 19), aes(x = months_elapsed, y = pct_vpd_lt_10, color = DAYNIGHT)) +
  geom_line(lwd = 1.2) +
  # geom_ribbon(aes(ymin = p10_vpd, ymax = p90_vpd), alpha = 0.2) +
  theme_bw() +
  labs(x = "Year",
       y = "Percent of fire detections at <10 hPa VPD",
       fill = "Day or Night detection",
       color = "Day or Night detection") +
  scale_fill_viridis_d(option = "E", direction = -1) +
  scale_color_viridis_d(option = "E", direction = -1) +
  # facet_grid(cols = vars(DAYNIGHT)) +
  geom_smooth(lty = 2, se = FALSE) +
  scale_x_continuous(breaks = seq(3, 220, by = 12), labels = 2001:2019) +
  facet_grid(rows = vars(hemisphere))

ggplot(afd_summary_months_elapsed_daynight_hemisphere %>% filter(months_elapsed > 19), aes(x = months_elapsed, y = n_vpd_lt_10, color = DAYNIGHT)) +
  geom_line(lwd = 1.2) +
  # geom_ribbon(aes(ymin = p10_vpd, ymax = p90_vpd), alpha = 0.2) +
  theme_bw() +
  labs(x = "Year",
       y = "Number of fire detections at <10 hPa VPD",
       fill = "Day or Night detection",
       color = "Day or Night detection") +
  scale_fill_viridis_d(option = "E", direction = -1) +
  scale_color_viridis_d(option = "E", direction = -1) +
  # facet_grid(cols = vars(DAYNIGHT)) +
  geom_smooth(lty = 2, se = FALSE) +
  scale_x_continuous(breaks = seq(3, 220, by = 12), labels = 2001:2019) +
  facet_grid(rows = vars(hemisphere))


# months elapsed, daynight ------------------------------------------------

afd_summary_months_elapsed_daynight <-
  afd_filtered[, .(mean_vpd = mean(vpd_hPa, na.rm = TRUE),
                   p10_vpd = tquantile(tdigest(vpd_hPa), probs = 0.1),
                   p90_vpd = tquantile(tdigest(vpd_hPa), probs = 0.9),
                   n_vpd_lt_10 = length(which(vpd_hPa < 10)),
                   n = length(vpd_hPa)),
               .(DAYNIGHT, months_elapsed)]

filler_months_elapsed_daynight <- 
  as.data.frame(expand.grid(acq_year = c(2003, 2006), 
                            acq_month = 1:12, 
                            DAYNIGHT = c("D", "N"))) %>% 
  dplyr::mutate(mean_vpd = NA, 
                p10_vpd = NA,
                p90_vpd = NA,
                months_elapsed = (acq_year - 2001) * 12 + 2 + acq_month,
                n_vpd_lt_10 = NA,
                n = NA)


afd_summary_months_elapsed_daynight <- 
  filler_months_elapsed_daynight %>% 
  dplyr::select(DAYNIGHT, months_elapsed, mean_vpd, p10_vpd, p90_vpd, n_vpd_lt_10, n) %>% 
  rbind(afd_summary_months_elapsed_daynight) %>% 
  dplyr::mutate(pct_vpd_lt_10 = n_vpd_lt_10 / n)


vpd_months_elapsed_daynight_gg <-
ggplot(afd_summary_months_elapsed_daynight %>% filter(months_elapsed > 19), aes(x = months_elapsed, y = mean_vpd, fill = DAYNIGHT, color = DAYNIGHT)) +
  geom_line(lwd = 1.2) +
  geom_ribbon(aes(ymin = p10_vpd, ymax = p90_vpd), alpha = 0.2) +
  theme_bw() +
  labs(x = "Year",
       y = "Vapor pressure deficit (hPa)",
       fill = "Day or Night detection",
       color = "Day or Night detection") +
  scale_fill_viridis_d(option = "E", direction = -1) +
  scale_color_viridis_d(option = "E", direction = -1) +
  # facet_grid(cols = vars(DAYNIGHT)) +
  geom_smooth(lty = 2, se = FALSE, col = "black") +
  scale_x_continuous(breaks = seq(3, 220, by = 12), labels = 2001:2019)

ggsave(filename = "figures/mean-vpd-thru-time-daynight.pdf", plot = vpd_months_elapsed_daynight_gg)




n_afd_thru_time_daynight_gg <-
ggplot(afd_summary_months_elapsed_daynight %>% filter(months_elapsed > 19), aes(x = months_elapsed, y = n, color = DAYNIGHT)) +
  geom_line(lwd = 1.2) +
  theme_bw() +
  labs(x = "Year",
       y = "Number of active fire detections per month",
       fill = "Day or Night detection",
       color = "Day or Night detection") +
  scale_fill_viridis_d(option = "E", direction = -1) +
  scale_color_viridis_d(option = "E", direction = -1) +
  geom_smooth(lty = 2, se = FALSE, color = "black", aes(group = DAYNIGHT)) +
  scale_x_continuous(breaks = seq(3, 220, by = 12), labels = 2001:2019)

ggsave("figures/n_afd_thru_time_daynight.pdf", plot = n_afd_thru_time_daynight_gg)

# daynight, hemisphere, year, month ---------------------------------------


afd_summary_month_year <-
  afd_filtered[, .(mean_vpd = mean(vpd_hPa, na.rm = TRUE),
                   p10_vpd = tquantile(tdigest(vpd_hPa), probs = 0.1),
                   p90_vpd = tquantile(tdigest(vpd_hPa), probs = 0.9),
                   n_vpd_lt_10 = length(which(vpd_hPa < 10)),
                   n_vpd_lt_05 = length(which(vpd_hPa < 5)),
                   n_vpd_lt_02 = length(which(vpd_hPa < 2)),
                   n = length(vpd_hPa)),
               .(DAYNIGHT, hemisphere, acq_month, acq_year)] %>% 
  dplyr::mutate(months_elapsed = (acq_year - 2001) * 12 + 2 + acq_month) %>% 
  dplyr::mutate(pct_vpd_lt_10 = n_vpd_lt_10 / n)

ggplot(afd_summary_month_year %>% dplyr::filter(months_elapsed > 19), aes(x = acq_month, y = mean_vpd, color = as.factor(acq_year), lty = DAYNIGHT)) +
  geom_line(lwd = 1.5) +
  scale_color_viridis_d() +
  facet_grid(cols = vars(hemisphere)) +
  theme_bw() +
  scale_x_continuous(breaks = seq(1, 12, by = 1)) +
  labs(color = "Year",
       lty = "Day or Night",
       x = "Month of year",
       y = "Mean Vapor Pressure Deficit (hPa)")

ggsave(filename = "figures/mean-vpd-by-month-year-daynight-hemisphere.pdf")

n_afd_vpd_lt_10_hPa_monthly_yearly_daynight_hemisphere <-
ggplot(afd_summary_month_year %>% dplyr::filter(months_elapsed > 19), aes(x = acq_month, y = n_vpd_lt_10, color = as.factor(acq_year), lty = DAYNIGHT)) +
  geom_line(lwd = 1.5) +
  scale_color_viridis_d() +
  facet_grid(cols = vars(hemisphere)) +
  theme_bw() +
  scale_x_continuous(breaks = seq(1, 12, by = 1)) +
  labs(color = "Year",
       lty = "Day or Night",
       x = "Month of year",
       y = "Number of fire detections with VPD < 10 hPa")

ggsave(filename = "figures/n-afd_vpd-lt-10-hPa_monthly_yearly_daynight_hemisphere.pdf", plot = n_afd_vpd_lt_10_hPa_monthly_yearly_daynight_hemisphere)


n_afd_vpd_lt_05_hPa_monthly_yearly_daynight_hemisphere <-
  ggplot(afd_summary_month_year %>% dplyr::filter(months_elapsed > 19), aes(x = acq_month, y = n_vpd_lt_05, color = as.factor(acq_year), lty = DAYNIGHT)) +
  geom_line(lwd = 1.5) +
  scale_color_viridis_d() +
  facet_grid(cols = vars(hemisphere)) +
  theme_bw() +
  scale_x_continuous(breaks = seq(1, 12, by = 1)) +
  labs(color = "Year",
       lty = "Day or Night",
       x = "Month of year",
       y = "Number of fire detections with VPD < 5 hPa")

ggsave(filename = "figures/n-afd_vpd-lt-05-hPa_monthly_yearly_daynight_hemisphere.pdf", plot = n_afd_vpd_lt_05_hPa_monthly_yearly_daynight_hemisphere)

n_afd_vpd_lt_02_hPa_monthly_yearly_daynight_hemisphere <-
ggplot(afd_summary_month_year %>% dplyr::filter(months_elapsed > 19), aes(x = acq_month, y = n_vpd_lt_02, color = as.factor(acq_year), lty = DAYNIGHT)) +
  geom_line(lwd = 1.5) +
  scale_color_viridis_d() +
  facet_grid(cols = vars(hemisphere)) +
  theme_bw() +
  scale_x_continuous(breaks = seq(1, 12, by = 1)) +
  labs(color = "Year",
       lty = "Day or Night",
       x = "Month of year",
       y = "Number of fire detections with VPD < 2 hPa")

ggsave(filename = "figures/n-afd_vpd-lt-02-hPa_monthly_yearly_daynight_hemisphere.pdf", plot = n_afd_vpd_lt_02_hPa_monthly_yearly_daynight_hemisphere)


ggplot(afd_summary_month_year %>% dplyr::filter(months_elapsed > 19), aes(x = acq_month, y = pct_vpd_lt_10, color = as.factor(acq_year), lty = DAYNIGHT)) +
  geom_line(lwd = 1.5) +
  scale_color_viridis_d() +
  facet_grid(cols = vars(hemisphere)) +
  theme_bw() +
  scale_x_continuous(breaks = seq(1, 12, by = 1)) +
  labs(color = "Year",
       lty = "Day or Night",
       x = "Month of year",
       y = "Percent of fire detections with VPD < 10 hPa")



# N afd under mild conditions through time daynight -----------------------

n_afd_mild_vpd_thru_time_daynight_hemisphere_gg <-
  ggplot(afd_summary_month_year %>% dplyr::filter(acq_year >= 2004), aes(x = months_elapsed, y = n_vpd_lt_10, color = DAYNIGHT)) +
  geom_line(lwd = 1.5) +
  scale_color_viridis_d(direction = -1) +
  facet_grid(cols = vars(hemisphere)) +
  theme_bw() +
  scale_x_continuous(breaks = seq(3, 220, by = 12), labels = 2001:2019) +
  labs(lty = "Day or Night",
       x = "Month of year",
       y = "Number of fire detections with VPD < 10 hPa") +
  geom_smooth(method = "lm")

n_afd_mild_vpd_thru_time_daynight_hemisphere_gg

ggsave(filename = "figures/n_afd_mild_vpd_thru_time_daynight_hemisphere.pdf", plot = n_afd_per_month_year_daynight_hemisphere_gg)


afd_summary_month_year_noDAYNIGHT <-
  afd_summary_month_year %>% 
  dplyr::filter(months_elapsed > 19) %>% 
  group_by(hemisphere, acq_month, acq_year) %>% 
  summarize(n_vpd_lt_10 = sum(n_vpd_lt_10))

ggplot(afd_summary_month_year_noDAYNIGHT, aes(x = acq_month, y = n_vpd_lt_10, color = as.factor(acq_year))) +
  geom_line(lwd = 1.5) +
  scale_color_viridis_d() +
  facet_grid(cols = vars(hemisphere)) +
  theme_bw() +
  scale_x_continuous(breaks = seq(1, 12, by = 1)) +
  labs(color = "Year",
       x = "Month of year",
       y = "Number of fire detections with VPD < 10 hPa")


# afd_summary_TOD_year <-
#   afd_filtered[, .(mean_vpd = mean(vpd_hPa, na.rm = TRUE),
#                    p10_vpd = tquantile(tdigest(vpd_hPa), probs = 0.1),
#                    p90_vpd = tquantile(tdigest(vpd_hPa), probs = 0.9),
#                    n = length(vpd_hPa)),
#                .(local_solar_hour_decmin_round, acq_year)]
# 
# afd_summary_TOD_year_gg <-
#   afd_summary_TOD_year %>%
#   dplyr::mutate(mean_vpd = ifelse(n < 200, NA, mean_vpd))
# 
# ggplot(afd_summary_TOD_year_gg, aes(x = local_solar_hour_decmin_round, y = mean_vpd, color = as.factor(acq_year))) +
#   geom_line(lwd = 1.5) +
#   scale_color_viridis_d() +
#   theme_bw() +
#   scale_x_continuous(breaks = seq(0, 24, by = 1))

pdf("figures/distribution-of-local-solar-time-of-active-fire-detections.pdf")
plot(density(afd_filtered$local_hour_decmin), 
     main = "Density of local solar times of active fire detections", 
     xlab = "Local solar time (24 hour scale)",
     las = 1,
     xaxt = "n")
axis(side = 1, at = seq(0, 24, by = 2))
dev.off()

# for (year in c(2004:2005, 2007:2018)) {
# png(paste0("figures/distribution-of-local-solar-time-of-active-fire-detections_", year, ".png"))
# plot(density(afd_filtered[acq_year == year]$local_hour_decmin),
#      main = paste0("Density of local solar times of active fire detections (", year, ")"),
#      xlab = "Local solar time (24 hour scale)",
#      las = 1,
#      xaxt = "n",
#      ylim = c(0, 0.7),
#      xlim = c(0, 24))
# axis(side = 1, at = seq(0, 24, by = 2))
# dev.off()
# }


pdf("figures/distribution-of-solar-elevation-angle-of-active-fire-detections.pdf")
plot(density(afd_filtered$solar_elev_ang), 
     main = "Density of solar elevation angles of active fire detections", 
     xlab = "Local solar time (24 hour scale)",
     las = 1,
     xaxt = "n")
axis(side = 1, at = seq(-90, 90, by = 15))
dev.off()

