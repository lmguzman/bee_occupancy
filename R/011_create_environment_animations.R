library(ggplot2)
library(purrr)
library(dplyr)
library(sf)
library(cowplot)
library(tidyr)
library(stringr)
library(viridis)
library(transformr)
library(gifski)
library(gganimate)

## temperature
year_range <- c(1995, 2015)

climate_raw <- readRDS(paste0("clean_data/climate/climate_counties.rds"))

climate <- climate_raw %>% 
  map_df(~as.data.frame(.x)) %>% 
  filter(year >= year_range[1] & year <= year_range[2])

## prepare temperature ### 

climate_temp <- climate %>%
  #mutate(site = paste0("s", str_pad(str_remove(site,"s"), width = 3, pad = "0", side = 'left'))) %>%
  mutate(site = paste0("s_", state_county)) %>% 
  filter(variable == 'tmax' & month %in% c(7,8)) %>% 
  group_by(site, year) %>% 
  dplyr::summarise(max_t_year = max(values, na.rm = TRUE)/10) %>% 
  mutate(year = paste0("yr", year)) %>%
  filter(!is.infinite(max_t_year), !is.na(max_t_year)) %>% 
  ungroup() %>% 
  mutate(scaled_p = scale(max_t_year)) 


max_temperature <- climate %>%
  #mutate(site = paste0("s", str_pad(str_remove(site,"s"), width = 3, pad = "0", side = 'left'))) %>%
  mutate(site = paste0("s_", state_county)) %>% 
  filter(variable == 'tmax' & month %in% c(7,8)) %>% 
  group_by(site, year) %>% 
  dplyr::summarise(max_t_year = max(values, na.rm = TRUE)/10) %>% 
  mutate(year = paste0("yr", year)) %>%
  filter(!is.infinite(max_t_year), !is.na(max_t_year))

## load regions and sites

region_df <- readRDS("clean_data/sites/site_counties_region.rds")

counties <- readRDS("clean_data/sites/sites_counties.RDS") %>% 
  mutate(site = paste0("s_", state_county)) 

county_temperature <- counties %>% 
  full_join(max_temperature) %>% 
  mutate(year = str_remove(year, "yr"))

county_temperature_scaled <- counties %>% 
  full_join(climate_temp) %>% 
  mutate(year = str_remove(year, "yr"))

for(yr in seq(1995, 2015, 3)){
 
  ct_yr_temperature <- county_temperature_scaled %>% 
    filter(year %in% yr)
  
  temperature_max <- ggplot() +
    geom_sf(data = ct_yr_temperature, aes(fill = scaled_p), colour = 'black', size = 0.1) +
    ggtitle(str_remove(unique(ct_yr_temperature$year), "yr")) +
    theme_cowplot() +
    scale_fill_viridis(name = "Maximum \n Temperature", option = 'turbo') +
    theme(axis.title=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(), 
          axis.line = element_blank())
  
  temperature_max
    
  ggsave(temperature_max, filename = paste0("plots/environment/temperature/", yr, "_scaled.jpeg"))
   
}

## animate 


temperature_all <- ggplot(data = county_temperature) +
  geom_sf(aes(fill = max_t_year), colour = 'black', size = 0.1) +
  theme_cowplot() +
  scale_fill_viridis(name = "Maximum \n Temperature", option = 'turbo')  +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(), 
        axis.line = element_blank())

temp_animation <- temperature_all +
  transition_states(year, transition_length = 2, state_length = 1) +
  labs(title = "{closest_state}") 

animate(temp_animation, height = 500, width = 800)

anim_save("plots/environment/animations/temperature.gif")


##### precipitation #####

## prepare temperature ### 

climate_prec <- climate %>% 
  #mutate(site = paste0("s", str_pad(str_remove(site,"s"), width = 3, pad = "0", side = 'left'))) %>% 
  mutate(site = paste0("s_", state_county)) %>% 
  filter(variable == 'prec') %>% 
  group_by(site, year) %>% 
  summarise(mean_prec_year = sum(values, na.rm = TRUE)) %>% 
  mutate(year = paste0("yr", year)) %>%
  filter(!is.infinite(mean_prec_year), !is.na(mean_prec_year)) %>% 
  ungroup()

county_precipitation <- counties %>% 
  full_join(climate_prec) %>% 
  mutate(year = str_remove(year, "yr"))

for(yr in seq(1995, 2015, 3)){
  
  ct_yr_prec <- county_precipitation %>% 
    filter(year %in% yr)
  
  prec_plot <- ggplot() +
    geom_sf(data = ct_yr_prec, aes(fill = mean_prec_year), colour = 'black', size = 0.1) +
    ggtitle(str_remove(unique(ct_yr_prec$year), "yr")) +
    theme_cowplot() +
    scale_fill_viridis(name = "Maximum \n Temperature", option = 'viridis', direction = -1) 
  
  ggsave(prec_plot, filename = paste0("plots/environment/precipitation/", yr, "_sum.jpeg"))
  
}


prec_all <- ggplot(data = county_precipitation) +
  geom_sf(aes(fill = mean_prec_year), colour = 'black', size = 0.1) +
  theme_cowplot() +
  scale_fill_viridis(name = "Mean Annual Precipitation", option = 'viridis', direction = -1)  +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(), 
        axis.line = element_blank())

prec_animation <- prec_all +
  transition_states(year, transition_length = 2, state_length = 1) +
  labs(title = "{closest_state}") 

animate(prec_animation, height = 500, width = 800)

anim_save("plots/environment/animations/precipitation.gif")



##### agriculture #####


agriculture <- readRDS(paste0("clean_data/agriculture/agriculture_county.rds"))

counties <- readRDS("clean_data/sites/sites_counties.RDS") %>% 
  mutate(site = paste0("s_", state_county)) 

agriculture_mat <- agriculture %>% 
  mutate(site = paste0("s_", state_county)) %>%
  dplyr::select(-state_county) 

county_agriculture <- counties %>% 
  full_join(agriculture_mat)

for(yr in unique(county_precipitation$year)){
  
  ct_yr_ag <- county_agriculture %>% 
    filter(year %in% yr)
  
  ag_plot <- ggplot() +
    geom_sf(data = ct_yr_ag, aes(fill = percent_agriculture), colour = 'black', size = 0.1) +
    ggtitle(unique(ct_yr_ag$year)) +
    theme_cowplot() +
    scale_fill_viridis(name = "Percent Agriculture", option = 'viridis', direction = -1)  +
    theme(axis.title=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(), 
          axis.line = element_blank())
  
  ggsave( ag_plot, filename = paste0("plots/environment/agriculture/", yr, ".jpeg"))
  
}

agriculture_all <- ggplot(data = county_agriculture) +
  geom_sf(aes(fill = percent_agriculture), colour = 'black', size = 0.1) +
  theme_cowplot() +
  scale_fill_viridis(name = "Percent Agriculture", option = 'viridis', direction = -1)  +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(), 
        axis.line = element_blank())


agri_animation <- agriculture_all +
  transition_states(year, transition_length = 2, state_length = 1) +
  labs(title = "{closest_state}") 

animate(agri_animation, height = 500, width = 800)

anim_save("plots/environment/animations/agriculture.gif")


##### neonic distribution #####

year_range <- c(1995, 2015)

neonic_raw <- readRDS(paste0("clean_data/pesticide/neonics_US_county.rds")) %>% 
  filter(YEAR >= year_range[1] & YEAR <= year_range[2]) %>% 
  as.data.table()

counties <- readRDS("clean_data/sites/sites_counties.RDS") %>% 
  mutate(site = paste0("s_", state_county)) 


neonic_raw %>% 
  filter(COMPOUND == "IMIDACLOPRID") %>% 
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

yr <- 2005

ct_yr_nall <- county_neonic_all %>% 
  filter(YEAR %in% yr)
  #filter(COMPOUND != "Dinotefuran")

neonic_all_plot <- ggplot() +
  geom_sf(data = ct_yr_nall, aes(fill = pest_site), colour = 'black', size = 0.1) +
  ggtitle(unique(ct_yr_nall$YEAR)) +
  theme_cowplot() +
  facet_wrap(~COMPOUND) +
  scale_fill_viridis(name = "Pesticide Use", option = 'viridis', direction = -1)  +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(), 
        axis.line = element_blank())


ggsave(neonic_all_plot, filename = "plots/environment/pesticide/2005_raw.jpeg")


neonic_all_ld50 <- ggplot() +
  geom_sf(data = ct_yr_nall, aes(fill = pest_site_ld50), colour = 'black', size = 0.1) +
  ggtitle(unique(ct_yr_nall$YEAR)) +
  theme_cowplot() +
  facet_wrap(~COMPOUND) +
  scale_fill_viridis(name = "Pesticide Use LD50", option = 'viridis', direction = -1)  +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(), 
        axis.line = element_blank())


ggsave(neonic_all_ld50, filename = "plots/environment/pesticide/2005_LD50.jpeg")


county_neonic_summed

ct_yr_nsum <- county_neonic_summed %>% 
  filter(YEAR %in% yr) 

neonic_all_summed <- ggplot() +
  geom_sf(data = ct_yr_nsum, aes(fill = summed_pesticides), colour = 'black', size = 0.1) +
  ggtitle(unique(ct_yr_nall$YEAR)) +
  theme_cowplot() +
  scale_fill_viridis(name = "Pesticide Use Summed", option = 'viridis', direction = -1)  +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(), 
        axis.line = element_blank())


ggsave(neonic_all_summed, filename = "plots/environment/pesticide/2005_summed.jpeg")

neonic_all_log <- ggplot() +
  geom_sf(data = ct_yr_nsum, aes(fill = loged_summed_pest), colour = 'black', size = 0.1) +
  ggtitle(unique(ct_yr_nall$YEAR)) +
  theme_cowplot() +
  scale_fill_viridis(name = "Pesticide Use Logged", option = 'viridis', direction = -1)  +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(), 
        axis.line = element_blank())


ggsave(neonic_all_log, filename = "plots/environment/pesticide/2005_logged.jpeg")



for(yr in unique(county_neonic_summed$YEAR)){
  
  ct_yr_nsum <- county_neonic_summed %>% 
    filter(YEAR %in% yr)
  
  neonic_plot <- ggplot() +
    geom_sf(data = ct_yr_nsum, aes(fill = loged_summed_pest), colour = 'black', size = 0.1) +
    ggtitle(unique(ct_yr_nall$YEAR)) +
    theme_cowplot() +
    scale_fill_viridis(name = "Pesticide Use Logged", option = 'viridis', direction = -1) 
  
  ggsave(neonic_plot, filename = paste0("plots/environment/pesticide/", yr, "_logged.jpeg"))
  
}

neonic_plot_all <- ggplot(data = county_neonic_summed) +
  geom_sf(aes(fill = loged_summed_pest), colour = 'black', size = 0.1) +
  theme_cowplot() +
  scale_fill_viridis(name = "Pesticide Use Logged", option = 'viridis', direction = -1)  +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(), 
        axis.line = element_blank())


neonic_animation <- neonic_plot_all +
  transition_states(YEAR, transition_length = 2, state_length = 1) +
  labs(title = "{closest_state}") 

animate(neonic_animation, height = 500, width = 800)

anim_save("plots/environment/animations/neonic.gif")


### neonic 2013 and region ###

yr <- 2013

ct_yr_nsum <- county_neonic_summed %>% 
  filter(YEAR %in% yr)

region_df <- readRDS("clean_data/sites/site_counties_region.rds")

counties <- readRDS("clean_data/sites/sites_counties.RDS")

counties_region <- counties %>% 
  left_join(region_df) 

counties_split <- split(counties_region, counties_region$region)

regions_combined <- lapply(counties_split, st_union)

neonic_region <- ggplot() +
  geom_sf(data = ct_yr_nsum, aes(fill = loged_summed_pest), colour = 'grey', size = 0.1) +
  theme_cowplot() +
  scale_fill_viridis(name = "Pesticide Use 2013 \n (log scale)", option = 'viridis', direction = -1) +
  theme(legend.position = "bottom", 
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        axis.line = element_blank()) +
  geom_sf(data = regions_combined$NorthEast, fill = "transparent", colour = 'black') +
  geom_sf(data = regions_combined$SouthEast, fill = "transparent", colour = 'black') +
  geom_sf(data = regions_combined$West,fill = "transparent",  colour = 'black') +
  geom_sf(data = regions_combined$Center, fill = "transparent", colour = 'black')

ggsave(neonic_region, filename = "plots/environment/pesticide/neonic_region_logged.jpeg")


### plot regions

region_df <- readRDS("clean_data/sites/site_counties_region.rds")

counties <- readRDS("clean_data/sites/sites_counties.RDS")

counties_region <- counties %>% 
  left_join(region_df)

region_map <- ggplot(data = counties_region) +
  geom_sf(aes(fill = region), colour = 'black', size = 0.1) +
  theme_cowplot() +
  scale_fill_discrete(name = "Region")  +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(), 
        axis.line = element_blank())



ggsave(region_map, filename = "plots/environment/regions.jpeg")
