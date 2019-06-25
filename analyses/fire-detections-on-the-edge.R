# Purpose: Is the latest active fire detection at fire perimeters a nighttime detection?

# Dependencies
library(tidyverse)
library(sf)
library(raster)
library(purrr)
library(here)


# Read all the data
frap_meta <- st_read("data/data_raw/wildfire-severity_sierra-nevada-ca-usa_ypmc_1984-2017_fire-metadata.geoJSON", stringsAsFactors = FALSE) %>% st_transform(3310)

severity_adf_filepaths <-
  list.files(path = here::here("data/data_output/wildfire-severity-active-fire-detections_sierra-nevada-ca-usa_ypmc_1984-2017"),
             full.names = TRUE)

full_dataframe <- 
  severity_adf_filepaths %>% 
  map(.f = read.csv,
      stringsAsFactors = FALSE) %>% 
  bind_rows()

# Subset to pixels that had at least 1 active fire detection, that occurred within 1000 meters of the fire perimeter
# New column representing whether the final active fire detection was at night. (If no night detections, then 0, if
# last active fire detection was during the day, then 0)
# New column representing the total number of days with active fire detections for the event
# New column representing the number of days between each pixel's last active fire detection and the event's 
# first active fire detection
# Refactor management objective column contents to be more descriptive

edge_cases <-
  full_dataframe %>% 
  dplyr::filter(count_total > 0) %>% 
  dplyr::filter(dist_to_edge > 0 & dist_to_edge < 1000) %>% 
  dplyr::mutate(last_afd_was_at_night = case_when(is.na(lastACQ_night) ~ 0,
                                                  lastACQ_night == lastACQ ~ 1,
                                                  TRUE ~ 0)) %>% 
  dplyr::group_by(fire_id) %>% 
  dplyr::mutate(days_of_afd = (max(lastACQ) - min(firstACQ)) / 60 / 60 / 24) %>% 
  dplyr::mutate(days_since_first_afd = (lastACQ - min(firstACQ)) / 60 / 60 / 24) %>% 
  dplyr::left_join(st_drop_geometry(frap_meta), by = "fire_id") %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(objective = ifelse(objective == 1, yes = "suppression", no = "wfu"))


ggplot(edge_cases %>% dplyr::filter(days_since_first_afd > 0), aes(days_since_first_afd))

edge_case_wfu <-
  edge_cases %>% 
  # dplyr::filter(objective == 2) %>% 
  dplyr::filter(days_since_first_afd > 2)

wfu_event_level <-
  edge_case_wfu %>% group_by(alarm_year, objective, fire_id) %>% summarize(n = n(), 
                                                                sum_last_afd_was_at_night = sum(last_afd_was_at_night),
                                                                prop_last_afd_was_at_night = mean(last_afd_was_at_night)) %>% 
  ungroup() %>% 
  dplyr::mutate(years_since_start = alarm_year - min(alarm_year))

last_night_acq_year_gg <-
  ggplot(wfu_event_level %>% filter(!is.na(objective)), aes(alarm_year, prop_last_afd_was_at_night, color = objective)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  labs(x = "Year of fire",
       y = "Proportion of fire's non-ignition perimeter\nwith a final active fire detection at night")

ggsave(plot = last_night_acq_year_gg, filename = "figures/last_night_acq_year.png")

fm1 <- glm(cbind(sum_last_afd_was_at_night, n - sum_last_afd_was_at_night) ~ years_since_start*objective, data = wfu_event_level, family = binomial())
summary(fm1)
confint(fm1)
# wfu_event_level_w_preds <- 
#   wfu_event_level %>% 
#   dplyr::bind_cols(as_tibble(predict(fm1, se = TRUE)[1:2])) %>% 
#   dplyr::mutate(lwr = binomial()$linkinv(fit - (qt(0.025, df = df.residual(fm1), lower.tail = FALSE) * se.fit)),
#                 upr = binomial()$linkinv(fit + (qt(0.025, df = df.residual(fm1), lower.tail = FALSE) * se.fit)))
# 
# ggplot(wfu_event_level_w_preds, aes(years_since_start, prop_last_afd_was_at_night)) +
#   geom_point() +
#   geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2) +
#   geom_line(aes(years_since_start, binomial()$linkinv(fit)), color = "red") +
#   theme_bw()
