### Script to create sites based on counties ###


library(sf)
library(dplyr)
library(tidyr)

# Load shapefile for us counties

us_counties <- read_sf('/Volumes/Rasters/USC/bee_occupancy/raw_data/pesticide/cb_2018_us_county_20m/cb_2018_us_county_20m.shp')

## select only county and state codes

state_county_fp <- us_counties[,c("STATEFP", "COUNTYFP")]

spdf <- as_Spatial(state_county_fp)

## remove Alaska, Hawaii, etc and keep only contigous USA

cont_usa <- state_county_fp %>% 
  filter(!STATEFP %in% c("02", "15", "60", "66", "69", "72", "78")) %>% 
  unite("state_county", STATEFP, COUNTYFP, sep = "_")

saveRDS(cont_usa, "clean_data/sites/sites_counties.rds")

## calculate area 

area_cont_usa <- data.frame(state_county = cont_usa$state_county, area_m_2 =  st_area(cont_usa))

saveRDS(area_cont_usa, "clean_data/sites/area_counties.rds")


