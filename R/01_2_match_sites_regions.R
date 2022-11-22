library(sf)
library(dplyr)
library(tidyr)

regions <- read_sf("raw_data/ecoregions_1/")

sites <- readRDS("clean_data/sites/sites_counties.rds")

regions_section <- regions %>% 
  select(NA_L1CODE) %>% 
  filter(NA_L1CODE %in% as.character(c(5:13, 15))) %>% 
  mutate(region = case_when(NA_L1CODE %in% c("5", "8", "15") ~ "East", 
                            NA_L1CODE %in% c("6", "7", "10", "11", "12", "13") ~ "West", 
                            NA_L1CODE %in% c("9") ~ "Center")) %>% 
  select(region) %>% 
  st_union(by_feature = TRUE)

sites_transformed <- sites %>% 
  st_transform(st_crs(regions_section))

site_region_intersection <- st_intersection(sites_transformed, regions_section) 

region_for_site <- site_region_intersection %>% 
  st_drop_geometry() %>% 
  unique()

duplicated_sites <- site_region_intersection %>% 
  filter(state_county %in% region_for_site$state_county[duplicated(region_for_site$state_county)]) %>% 
  arrange(state_county) 

st_area(duplicated_sites)

area_duplicated_regions <- duplicated_sites %>% 
  mutate(area = st_area(duplicated_sites)) %>%
  st_drop_geometry() %>% 
  group_by(state_county, region) %>% 
  summarise(area = sum(area)) %>% 
  mutate(area = round(area/100000, 2)) %>% 
  pivot_wider(values_from = "area", names_from = "region") %>% 
  mutate(final_region = case_when(East > Center ~ "East", 
                                  West > Center ~ "West", 
                                  TRUE ~ "Center")) %>%
  select(state_county, region = final_region)

region_for_sites_final <- region_for_site %>% 
  filter(!state_county %in% region_for_site$state_county[duplicated(region_for_site$state_county)]) %>%
  bind_rows(area_duplicated_regions)

region_for_sites_final2 <- region_for_sites_final %>% 
  left_join(data.frame(state_county = sites$state_county, st_coordinates(st_centroid(sites)))) %>% 
  mutate(region2 = ifelse(region == "East" & Y > 37.5, 
                          "NorthEast", region)) %>% 
  mutate(region2 = ifelse(region2 == "East", "SouthEast", region2)) %>% 
  select(state_county, region = region2)

saveRDS(region_for_sites_final2, "clean_data/sites/site_counties_region.rds")


## plot regions ##

library(ggplot2)
library(sf)
library(dplyr)
library(cowplot)

region_for_sites_final <- readRDS("clean_data/sites/site_counties_region.rds")

region_for_sites_final$region %>% table()

sites <- readRDS("clean_data/sites/sites_counties.rds")

sites %>% 
  left_join(region_for_sites_final) %>% 
  ggplot() +
  geom_sf(aes(fill = region)) +
  theme_cowplot()


## some sites in the center and on the north east could belong to other areas ##
## manually check those counties and put them in the correct areas ##

library(ggsflabel)

sites %>% 
  left_join(filter(region_for_sites_final, region == 'West')) %>% 
  filter(!is.na(region)) %>% 
  ggplot() +
  geom_sf(aes(fill = region)) +
  geom_sf_label(aes(label = state_county), size = 2) +
  theme_cowplot() 

## 56_011, 46_033, 46_081 should be in the Center


sites %>% 
  left_join(filter(region_for_sites_final, region == 'Center')) %>% 
  filter(!is.na(region)) %>% 
  ggplot() +
  geom_sf(aes(fill = region)) +
  geom_sf_label(aes(label = state_county), size = 1) +
  theme_cowplot() 

## 55_109, 27_037 should be in NE

## update data with those regions 

region_for_sites_final3 <- region_for_sites_final2 %>% 
  mutate(region = ifelse(state_county %in% c("56_011", "46_033", "46_081"), "Center", region)) %>% 
  mutate(region= ifelse(state_county %in% c("55_109", "27_037"), "NorthEast", region))

saveRDS(region_for_sites_final3, "clean_data/sites/site_counties_region.rds")

## check plot

region_for_sites_final <- readRDS("clean_data/sites/site_counties_region.rds")

sites <- readRDS("clean_data/sites/sites_counties.rds")

sites %>% 
  left_join(region_for_sites_final) %>% 
  ggplot() +
  geom_sf(aes(fill = region)) +
  theme_cowplot()
