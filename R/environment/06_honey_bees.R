#### script to get honey bee data 

library(dplyr)
library(sf)
library(stringr)
library(lubridate)
library(purrr)
library(tidyr)
library(ggplot2) 
library(cowplot)
library(data.table)

######### process data from the NASS survey of agriculture #########

inventory <- read.csv("raw_data/honey_bees/USDA_CENSUS_OTHER/1CB32B5E-9911-3C0C-B5C3-FF5EDF06E363.csv", na.strings = ' (D)') %>% 
  filter(Data.Item == "HONEY, BEE COLONIES - INVENTORY, MEASURED IN COLONIES") %>% 
    mutate(`CV....` = ifelse(`CV....` %in% c("", "(L)", "(H)"), NA, `CV....`)) %>%
    mutate(`CV....` = as.numeric(`CV....`)) %>% 
  mutate(Value = as.numeric(str_remove(Value, ","))) %>% 
  filter(!is.na(County.ANSI)) %>% 
  mutate(state_county = paste0(str_pad(State.ANSI, 2, "left", "0"), "_", str_pad(County.ANSI, 3, "left", "0"))) %>% 
  dplyr::select(Year, state_county, Value) %>% 
  mutate(Value = ifelse(is.na(Value), 0, Value))

saveRDS(inventory, "clean_data/honey_bees/colonies_time.rds")



## check distribution across states

us_counties <- read_sf('/Volumes/Rasters/USC/bee_occupancy/raw_data/counties_states/cb_2018_us_state_20m/')

## select only county and state codes

state_county_fp <- us_counties[,c("STATEFP", "STUSPS")]

inventory %>% 
  mutate(STATEFP = str_sub(state_county, end = -5)) %>% 
  left_join(state_county_fp) %>% 
  st_drop_geometry() %>% 
  filter(STATEFP != 15) %>% 
  group_by(STUSPS, Year) %>% 
  summarise(sum_col = sum(Value)) %>% 
  left_join(state_county_fp) %>% 
  dplyr::select(geometry, Year, sum_col) %>% 
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = sum_col)) +
  facet_wrap(~Year) +
  theme_cowplot() +
  labs(fill = "Total Number\nColonies")
