
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




#pest_all_scaled <- (pest_all- mean(pest_all))/sd(pest_all)


## compare for 1 year

#yr <- 2005

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
    ggtitle(unique(ct_yr_nsum$YEAR)) +
    theme_cowplot() +
    scale_fill_viridis(name = "Pesticide Use Logged", option = 'viridis', direction = -1) 
  
  ggsave(neonic_plot, filename = paste0("plots/environment/pesticide/", yr, "_logged.jpeg"))
  
}

county_neonic_summed_all <- county_neonic_summed %>% st_sf()

neonic_plot_all <- ggplot(data = county_neonic_summed_all) +
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

