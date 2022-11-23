library(dplyr)
library(sf)
library(ggplot2)

region_filter <- "West"

regions <- readRDS("clean_data/sites/site_counties_region.rds")

sites <- readRDS("clean_data/sites/sites_counties.RDS")

site_in_region <- filter(sites, state_county %in% filter(regions, region == region_filter)$state_county)

## load in here the observations in the west

ggplot() +
  geom_sf(data = site_in_region) +
  theme_cowplot() +
  geom_point(data = observations, aes(x = finalLongitude, y = finalLatitude, colour = year), size = 2)

observations_clean_sp <- read.csv(paste0("clean_data/observations_used/", region_filter, ".csv"))

# get number of species per site

species_richness <- observations_clean_sp %>% 
  select(site, finalName) %>% 
  distinct() %>% 
  group_by(site) %>%
  summarise(n_sp = n())
  
