library(sf)

fired <- st_read("C:/Users/mikoo/Downloads/modis_event_polygons_cus.gpkg") %>% st_transform(4326)

fired_shp <-
  fired %>% 
  dplyr::rename(perim_leng = final_perimeter,
                total_px = total_pixels,
                ig_date = ignition_date,
                ig_doy = ignition_doy,
                ig_month = ignition_month,
                ig_year = ignition_year,
                ig_lat = ignition_latitude,
                ig_lon = ignition_longitude,
                ig_state = ignition_state,
                area_km2 = total_area_km2,
                area_ac = total_area_acres,
                area_ha = total_area_ha,
                fsr_px = fsr_pixels_per_day,
                fsr_km2 = fsr_km2_per_day,
                fsr_ac = fsr_acres_per_day,
                fsr_ha = fsr_ha_per_day,
                mx_grw_px = max_growth_pixels,
                mx_grw_km2 = max_growth_km2,
                mx_grw_ac = max_growth_acres,
                mx_grw_ha = max_growth_ha,
                mi_grw_px = min_growth_pixels,
                mi_grw_km2 = min_growth_km2,
                mi_grw_ac = min_growth_acres,
                mi_grw_ha = min_growth_ha,
                mu_grw_px = mean_growth_pixels,
                mu_grw_km2 = mean_growth_km2,
                mu_grw_ac = mean_growth_acres,
                mu_grw_ha = mean_growth_ha,
                mx_grw_dt = max_growth_date,
                l1_ecorg = l1_ecoregion)

st_write(fired_shp, "C:/Users/mikoo/Downloads/modis_event_polygons_cus.shp", delete_dsn = TRUE)