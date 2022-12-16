library(dplyr)
library(ggplot2)
library(purrr)
library(cowplot)

## load in data##

ranges <- readRDS("clean_data/ranges/ranges_counties.rds")

regions <- readRDS("clean_data/sites/site_counties_region.rds")

area <- readRDS("clean_data/sites/area_counties.RDS")

range_df <- ranges %>% 
  map_df(~as.data.frame(.x), .id = 'species') %>% 
  rename(state_county = `.x`)

## number of species per region

species_by_region <- range_df %>% 
  left_join(regions) %>% 
  select(region, species) %>% 
  unique()
  
n_species_by_region <- species_by_region %>% 
  group_by(region) %>% 
  summarise(n = n())

## number of counties per region 

n_county_region <- regions %>% 
  group_by(region) %>% 
  summarise(n = n())

### distribution of counties per region -- how many counties are in each species range per region

n_counties_species_region <- range_df %>% 
  left_join(regions) %>% 
  group_by(species, region) %>% 
  unique() %>% 
  summarise(n = n())

n_counties_species_region %>% 
  ggplot(aes(x = n)) +
  facet_wrap(~region) +
  geom_histogram() +
  theme_cowplot() +
  xlab("Number of counties per species") +
  theme(strip.background = element_blank()) +
  geom_text(data = n_species_by_region, aes(y = 200, x = 700, label = paste("N species =", n))) +
  geom_text(data = n_county_region, aes(y = 150, x = 700, label = paste("N counties =", n)))

## area of species ranges per region 

area_species_region <- range_df %>% 
  left_join(regions) %>% 
  left_join(area) %>% 
  group_by(species, region) %>% 
  unique() %>% 
  summarise(area_range = sum(area_m_2) * 1e-6)


area_species_region %>% 
  ggplot(aes(x = area_range)) +
  facet_wrap(~region) +
  geom_histogram() +
  theme_cowplot() +
  xlab("Area of counties per species Km2") +
  theme(strip.background = element_blank()) +
  geom_text(data = n_species_by_region, aes(y = 100, x = 1.5e+6, label = paste("N species =", n))) +
  geom_text(data = n_county_region, aes(y = 80, x = 1.5e+6, label = paste("N counties =", n)))
