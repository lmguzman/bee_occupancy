library(dplyr)
library(sf)
library(stringr)
library(lubridate)
library(purrr)
library(tidyr)
library(ggplot2) 
library(cowplot)
library(data.table)

## data from https://usda.library.cornell.edu/concern/publications/rn301137d
## 2016

all_files <- list.files("raw_data/honey_bees/BeeColonies-05-12-2016/", full.names = TRUE)

files_to_read <- all_files[str_detect(all_files, '.csv')][2:5]

potential_months <- as.character(lubridate::month(1:12, label = TRUE))

colonies_all <- list()

for(f in files_to_read){
  
  honey_bee <- read.csv(f, skip = 8)
  
  first_line <- paste(read.table(f, skip = 3, nrow = 1), collapse = " ")
  
  chosen_months <- potential_months[str_detect(first_line, potential_months)]
  
  colnames(honey_bee)
  
  colonies_all[[f]] <- honey_bee[1:51,c(3,4)] %>% 
    filter(!is.na(X.number.)) %>% 
    rename("State" = X, "Colonies_start" = `X.number.`) %>% 
    mutate(Month = chosen_months[1])
  
}

colonies_state <- colonies_all %>% 
  map_df(~as.data.frame(.x)) %>% 
  pivot_wider(names_from = "Month", values_from = "Colonies_start")

##### Plot colonies across the country

states <- read_sf("raw_data/counties_states/cb_2018_us_state_20m/") %>% 
  filter(!NAME %in% c("Alaska", "Hawaii", "Puerto Rico"))

states_in_other <- states %>% 
  anti_join(colonies_state, c('NAME' = "State")) %>% 
  dplyr::select(NAME) %>% st_drop_geometry() %>% 
  rename(State = NAME)

other_five_states <- colonies_state %>% filter(str_detect(State, "Other")) %>% 
  mutate_if(is.numeric, function(x) x/5) %>% 
  select(-State) %>% 
  bind_cols(states_in_other)

all_colonies <- bind_rows(filter(colonies_state, !str_detect(State, "Other")), other_five_states)

write.csv(all_colonies, "clean_data/honey_bees/colonies_state_2016.csv", row.names = FALSE)


states_colonies <- states %>% 
  left_join(all_colonies,
            c('NAME' = "State"))
  
ggplot() +
  geom_sf(data = states_colonies, aes(fill = log(Jan))) +
  theme_cowplot() +
  scale_fill_viridis_c("Number of Honey\n Bee colonies (log)")

## calculate colonies per area

area_state <- st_area(states)

colonies_per_area <- states_colonies %>% 
  st_drop_geometry() %>% 
  select(-c(Apr:Oct)) %>% 
  mutate(area_state = area_state) %>% 
  mutate(n_colonies_per_unit = Jan/area_state)

## calculate the colonies per county based on 1 -  same amount to all counties, 2 - amount * canag area

## method 1, divide the number of colonies by the number of counties

counties_all <- readRDS("clean_data/sites/sites_counties.RDS") %>% 
  st_drop_geometry() %>% 
  tidyr::separate(state_county, c("state", "county"), sep =  "_", remove = FALSE) %>% 
  mutate(site = paste0("s_", state_county))

n_counties_state <- counties_all %>% 
  count(state) %>% 
  left_join(states %>% 
              select(STATEFP, NAME) %>% 
              st_drop_geometry(), by = c('state' = 'STATEFP')) %>% 
  left_join(all_colonies, by = c('NAME' = 'State')) %>% 
  mutate(col_county = Jan/n)

colonies_county_simple <- counties_all %>% 
  left_join(n_counties_state) %>% 
  select(site, col_county)

saveRDS(colonies_county_simple, "clean_data/honey_bees/colonies_county_simple.rds")

## method 2, multiply the proportion of animal agriculture by the number of colonies

crop_county_animal <- readRDS(paste0("clean_data/agriculture/crops_county_animal.rds"))

## calcualte total area
Total_county_area <- crop_county_animal %>% 
  group_by(site) %>% 
  summarise(total_area = sum(Freq))

## calculate animal pollinated area
Total_animal_pollinated <- crop_county_animal %>% 
  filter(cover_type == 'Crop') %>% 
  filter(non_animal_pollinated %in% c("FALSE", "HALF")) %>% 
  group_by(site) %>% 
  summarise(total_ani_pollinated = sum(Freq))

## calculate fraction of animal pollinated
frac_animal_pollinated_all <- Total_county_area %>% 
  left_join(Total_animal_pollinated) %>% 
  mutate(county_animal_pol = total_ani_pollinated/total_area) %>%
  mutate(county_animal_pol = ifelse(is.na(county_animal_pol), 0, county_animal_pol))  %>% 
  select(site, county_animal_pol)

## join with number of colonies

colines_state_number <- states %>% 
  select(STATEFP, NAME) %>% 
  st_drop_geometry() %>% 
  left_join(all_colonies, by = c('NAME' = 'State'))

colonies_county_canag <- frac_animal_pollinated_all %>% 
  mutate(site = str_remove(site, "s_")) %>% 
  tidyr::separate(site, c("state", "county"), sep =  "_", remove = FALSE) %>% 
  mutate(site = paste0("s_", site)) %>% 
  left_join(colines_state_number, by = c('state' = 'STATEFP')) %>% 
  mutate(col_county = county_animal_pol * Jan) %>% 
  select(site, col_county)
  
saveRDS(colonies_county_canag, "clean_data/honey_bees/colonies_county_canag.rds")


counties_shapefile <- readRDS("clean_data/sites/sites_counties.RDS")

colonies_county_canag %>% 
  mutate(state_county = str_remove(site, "s_"))
  

counties_shapefile %>% 
  left_join(colonies_county_canag %>% 
              mutate(state_county = str_remove(site, "s_"))) %>% 
  ggplot() +
  geom_sf(aes(fill = log(colonies_an_pol))) +
  scale_fill_viridis()

###### colonies per agri-region #######

region_df <- readRDS("clean_data/sites/site_counties_agriregion.rds") %>% 
  mutate(region_collapsed = case_when(region %in% c("Southern Seaboard", "Eastern Uplands",
                                                    "Mississippi Portal") ~ "South East",
                                      region %in% c("Heartland", "Prairie Gateway") ~ "Central",
                                      TRUE ~ region))

counties_shp <- readRDS("clean_data/sites/sites_counties.RDS")

county_region <- counties_shp %>% 
  left_join(region_df) 

region_shapefiles <- split(county_region, county_region$region_collapsed) %>%
  map(~st_union(.x))
  

###

total_colonies_region <- data.frame()

for(r in 1:6){
  
  state_region <- st_intersection(states, region_shapefiles[[r]])
  
  ggplot() + 
    geom_sf(data = region_shapefiles[[r]]) +
    geom_sf(data = state_region)
  
  state_region$area_section <- st_area(st_make_valid(state_region))
  
  
  colonies_region <- state_region %>% 
    left_join(colonies_per_area) %>% 
    mutate(expected_colonies = n_colonies_per_unit * area_section) 
  
  total_colonies_region <- rbind(total_colonies_region, data.frame(colonies_region = sum(colonies_region$expected_colonies), region = names(region_shapefiles[r])))
}

total_colonies_region %>% 
  arrange(colonies_region)

###### look at gbif honey bee data ####

honey_bees <- fread("raw_data/honey_bees/0233178-230224095556074.csv")

honey_bees_fil <- honey_bees[occurrenceStatus == "PRESENT" & !is.na(decimalLongitude) & !is.na(decimalLatitude)]

lat_lon <- honey_bees_fil %>% 
  st_as_sf(
    coords = c("decimalLongitude", "decimalLatitude"),
    agr = "constant",
    #crs = 4326,  ##WGS84   
    crs = 4269,
    stringsAsFactors = FALSE,
    remove = FALSE) 

# get county for each observation

lat_lon %>% 
  ggplot() + geom_sf()

site_obs <- st_join(lat_lon, counties_shp, join = st_within)

n_obs_county <- site_obs %>% 
  group_by(state_county) %>% 
  count() %>% 
  st_drop_geometry()

counties_shp %>% 
  left_join(n_obs_county) %>% 
  ggplot() + geom_sf(aes(fill = log(n))) +
  scale_fill_viridis()


######### USDA Quick Stats -- not sure if this is good! lots of missing data #########

inventory <- read.csv("raw_data/honey_bees/CC06DEB4-F568-3018-8353-B393282830EC.csv", na.strings = '(D)') %>% 
  mutate(`CV....` = ifelse(`CV....` %in% c("", "(L)", "(H)"), NA, `CV....`)) %>% 
  mutate(`CV....` = as.numeric(`CV....`))
inventory_other <- read.csv("raw_data/honey_bees/D43100D6-CB1C-3C2B-B1FE-6C5BD6CDBDC3.csv", na.strings = "(D)")

inventory_clean <- inventory %>% bind_rows(inventory_other) %>% 
  dplyr::select(Year, State, State.ANSI, County, County.ANSI, Value, `CV....`) %>% 
  rename("CV" = `CV....`) %>% 
  mutate(Value = ifelse(Value == ' (D)', NA, Value)) %>% 
  mutate(Value = as.numeric(str_remove(Value, ","))) %>% 
  mutate(state_county = paste0(str_pad(State.ANSI, 2, "left", "0"), "_", str_pad(County.ANSI, 3, "left", "0"))) %>% 
  filter(Year == 2012)

counties_shp %>% 
  left_join(inventory_clean) %>% 
  ggplot() + geom_sf(aes(fill = log(Value))) +
  scale_fill_viridis()

inventory_clean %>% 
  left_join(region_df) %>% filter(!is.na(region_collapsed)) %>% 
  ggplot(aes(x = region_collapsed, y = Value)) + 
  geom_violin() +
  geom_jitter() +
  scale_y_log10()

inventory_clean %>% 
  left_join(region_df) %>% filter(!is.na(region_collapsed)) %>% 
  group_by(region_collapsed) %>% 
  summarize(mean = mean(Value, na.rm = TRUE), sd = sd(Value, na.rm = TRUE), sum = sum(Value, na.rm = TRUE))  %>% 
  arrange(desc(sum))
  

## try 1

##### give each county the same value##

## try 2

### or the fraction of animal pollinated agriculture in the county * total number of colonies in a state
## adds collinearity within a state but not necessarily across state

## honey bees alone
## pesticide + honey bee
## pesticide + honey bee + animal pollinated agriculture 

## how do farmers change behavior with honey bees and without honey bees
## figure out why basin and range goes down that one year
## get crop data layer for other years



