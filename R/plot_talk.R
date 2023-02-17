library(dplyr)
library(sf)
library(ggplot2)
library(viridis)

observations_raw <- readRDS(paste0("clean_data/observations/observations_counties_geometry.rds"))

sp_observed_county <- observations_raw %>% 
  select(finalName, state_county) %>% 
  st_drop_geometry() %>% 
  unique() %>% 
  group_by(state_county) %>% 
  summarise(n_sp = n())

sp_observed_county %>% 
  arrange(desc(n_sp))

region_df <- readRDS("clean_data/sites/site_counties_region.rds")

counties <- readRDS("clean_data/sites/sites_counties.RDS")

nsp_county <- counties %>% 
  left_join(sp_observed_county) 
  #mutate(n_sp = ifelse(is.na(n_sp), 0, n_sp))

counties_region <- counties %>% 
  left_join(region_df) 

counties_split <- split(counties_region, counties_region$region)

regions_combined <- lapply(counties_split, st_union)


nsp_region <- ggplot() +
  geom_sf(data = nsp_county, aes(fill = n_sp), colour = 'grey',size = 0.1) +
  theme_cowplot() +
  scale_fill_viridis(name = "Number of species \n observed", option = 'viridis', direction = -1) +
  theme(legend.position = "bottom", 
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        axis.line = element_blank()) +
  geom_sf(data = regions_combined$NorthEast, fill = "transparent", colour = 'black', size = 0.9) +
  geom_sf(data = regions_combined$SouthEast, fill = "transparent", colour = 'black', size = 0.9) +
  geom_sf(data = regions_combined$West,fill = "transparent",  colour = 'black', size = 0.9) +
  geom_sf(data = regions_combined$Center, fill = "transparent", colour = 'black', size = 0.9)

ggsave(nsp_region, filename = "plots/n_species.jpeg")



#### plot neonic only where there are species ####


##### neonic distribution #####

year_range <- c(1995, 2015)

neonic_raw <- readRDS(paste0("clean_data/pesticide/neonics_US_county.rds")) %>% 
  filter(YEAR >= year_range[1] & YEAR <= year_range[2]) %>% 
  as.data.table()

counties <- readRDS("clean_data/sites/sites_counties.RDS") %>% 
  mutate(site = paste0("s_", state_county)) 

## fill in gaps for sites where no pesticide use detected 

year_site <- expand.grid(YEAR = year_range[1]:year_range[2], state_county = counties$state_county,
                         COMPOUND = unique(neonic_raw$COMPOUND)) %>% 
  data.table()

setkeyv(year_site, c("YEAR", "state_county", "COMPOUND"))
setkeyv(neonic_raw,  c("YEAR", "state_county", "COMPOUND"))

neonic_all_sites <- neonic_raw[year_site] %>% 
  mutate(pest_site_ld50 = ifelse(is.na(pest_site_ld50), 0, pest_site_ld50))   %>% 
  mutate(site = paste0("s_", state_county))

neonic_sum_log <- neonic_all_sites %>% 
  group_by(site, YEAR) %>% 
  summarise(summed_pesticides = sum(pest_site_ld50, na.rm = TRUE)) %>% 
  mutate(loged_summed_pest = log(summed_pesticides + 1))

county_neonic_all <- counties %>% 
  full_join(neonic_all_sites) %>% 
  mutate(COMPOUND = str_to_title(COMPOUND))

county_neonic_summed <- counties %>% 
  full_join(neonic_sum_log)

#pest_all_scaled <- (pest_all- mean(pest_all))/sd(pest_all)


## compare for 1 year

yr <- 2013

ct_yr_nsum <- county_neonic_summed %>% 
  filter(YEAR %in% yr)

region_df <- readRDS("clean_data/sites/site_counties_region.rds")

counties <- readRDS("clean_data/sites/sites_counties.RDS")

counties_region <- counties %>% 
  left_join(region_df) 

counties_split <- split(counties_region, counties_region$region)

regions_combined <- lapply(counties_split, st_union)


  

pest_detect <- ct_yr_nsum %>% 
  left_join(st_drop_geometry(nsp_county)) %>% 
  mutate(pest_na = ifelse(loged_summed_pest == 0, NA, loged_summed_pest)) 
  


neonic_region <- ggplot() +
  geom_sf(data = pest_detect, aes(fill = loged_summed_pest * n_sp), colour = 'grey', size = 0.1) +
  theme_cowplot() +
  scale_fill_viridis(name = "Pesticide Use 2013 \n (log scale)", option = 'viridis', direction = -1) +
  theme(legend.position = "bottom", 
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        axis.line = element_blank()) +
  geom_sf(data = regions_combined$NorthEast, fill = "transparent", colour = 'black', size = 0.9) +
  geom_sf(data = regions_combined$SouthEast, fill = "transparent", colour = 'black', size = 0.9) +
  geom_sf(data = regions_combined$West,fill = "transparent",  colour = 'black', size = 0.9) +
  geom_sf(data = regions_combined$Center, fill = "transparent", colour = 'black', size = 0.9)


pest_detect %>% 
  #filter(!is.na(n_sp)) %>% 
  left_join(region_df) %>% 
  group_by(region) %>% 
  summarise(n())

pest_detect %>% 
  filter(!is.na(n_sp)) %>% 
  left_join(region_df) %>% 
  ggplot(aes(x = loged_summed_pest, y = n_sp))  +
  geom_point() +
  facet_wrap(~region)


pest_detect %>% 
  filter(!is.na(n_sp) & !is.na(pest_na)) %>% 
  left_join(region_df) %>% 
  ggplot(aes(x = loged_summed_pest, y = n_sp))  +
  geom_point() +
  facet_wrap(~region) +
  scale_y_log10()
