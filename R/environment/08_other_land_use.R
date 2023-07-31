### script to process natural areas ###

library(sf)
library(dplyr)
library(raster)
library(data.table)
library(stringr)
library(ggplot2)
library(tidyr)
library(purrr)
library(cowplot)
library(broom)

### load the sites 

sites <- readRDS("clean_data/sites/sites_counties.RDS")

## extract all of the land_use types in the each county (from the nlcd dataset)
## note this takes a long time!

for(year in c(2001, 2004, 2006, 2008, 2011, 2013)){
  
  land_cover <- raster(paste0('/Volumes/Rasters/USC/bee_occupancy/raw_data/agriculture/nlcd_', year, '_land_cover_l48_20210604/nlcd_',year,'_land_cover_l48_20210604.img'))
  
  for(i in 1:nrow(sites)){
    land_cover_site <- extract(land_cover, sites[i,])
    saveRDS(land_cover_site, paste0('clean_data/land_use/land_use_all/land_use_county',sites[i,]$state_county,"_",year,'.rds'))
  }     
}

## load and aggregate

county_land_use <- list.files('clean_data/land_use/land_use_all/', full.names = TRUE)

county_land_use <- county_land_use[str_detect(county_land_use, "2001|2016")]

all_land_use <- list()

counter <- 1

for(land_use_file in county_land_use){
  
  county <- str_extract(land_use_file, "\\d+\\_\\d+")
  year <- str_extract(land_use_file, "2\\d\\d\\d")
  land_use_site <- readRDS(land_use_file)
  all_land_use[[counter]] <- data.frame(table(land_use_site), state_county = county, year = year)
  counter <- counter+1
  print(counter)
}

land_use_all_year <-rbindlist(all_land_use)

saveRDS(land_use_all_year, 'clean_data/land_use/land_use_county2001_2016.rds')

#### assign multiple categories to "natural and semi-natural areas" ###

land_use_all_year <- readRDS('clean_data/land_use/land_use_county2001_2016.rds') %>% 
  mutate(year = as.numeric(year))

land_use_categories <- read.csv("clean_data/land_use/land_use_cats.csv") %>% 
  mutate(land_use = str_trim(land_use)) %>% 
  mutate(land_use_site = factor(value)) %>% 
  mutate(land_use_collapsed = case_when(land_use %in% c("Deciduous Forest", "Evergreen Forest", "Mixed Forest", "Woody Wetlands", 
                                                        "Emergent Herbaceous Wetlands",
                                                        "Shrub/Scrub", "Grassland/Herbaceous", "Sedge/Herbaceous", "Pasture/Hay") ~ "Natural/semi-natural",
                                        TRUE ~ "Other"))

land_use_all_cats <- land_use_all_year %>% 
  left_join(land_use_categories) 

## Summarise to get the percent of natural area per county

county_total <- land_use_all_cats %>% 
  group_by(state_county, year) %>% 
  summarise(total = sum(Freq))

county_land_use <- land_use_all_cats %>% 
  group_by(state_county, year, land_use_collapsed) %>% 
  summarise(land_use_total = sum(Freq))

percent_county_land_use <- county_land_use %>% 
  left_join(county_total) %>% 
  mutate(percent_county = land_use_total/total) %>% 
  mutate(site = paste0("s_", state_county))

### Check how many counties have lost natural areas more than 5%

percent_county_land_use %>% 
  filter(land_use_collapsed == "Natural/semi-natural") %>% 
  dplyr::select(year, state_county, percent_county) %>% 
  pivot_wider(names_from = 'year', values_from = 'percent_county') %>% 
  mutate(net_change = abs(`2016` - `2001`)) %>% 
  filter(net_change > 0.05) %>% nrow()
  dplyr::select(-net_change) %>% 
  pivot_longer(names_to = 'year', values_to = 'prop_nat', -state_county) %>% 
  ggplot() +
  geom_line(aes(x = year, y = prop_nat, group = state_county, colour = state_county)) +
  theme_cowplot() +
  theme(legend.position = 'none') +
  ylab("Propotion of the county that is natural area")
  
######### check whether natural areas were lost in areas of agriculture or pesticide use#####
  
#note this second half of the script can only be run once the main script 05 (compiling the environment) is run

### check correlations with animal pollinated agriculture, or pesticide

## load environmental data  

env_all <- readRDS("clean_data/data_prepared/environment_counties_1995_2015.rds")

## joing animal pollinated agriculture and natural area

canag_percent_nat <- percent_county_land_use %>% 
  filter(land_use_collapsed == "Natural/semi-natural") %>% 
  ungroup() %>% 
  dplyr::select(site, year, percent_county) %>%
  pivot_wider(names_from = 'year', values_from = 'percent_county') %>%
  left_join(data.frame(canag = env_all$canag_col_mat) %>% 
              tibble::rownames_to_column("site")) %>% 
  tibble::column_to_rownames("site") %>% 
  as.matrix()
  
## correlation between loss in natural area and agriculture

tidy(cor.test(canag_percent_nat[,3], (canag_percent_nat[,2] - canag_percent_nat[,1])))

## correlation between natural area and agriculture

cor(canag_percent_nat)

## joing pesticide use and natural area

pest_long <- env_all$both_mat_area %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column("site") %>% 
  dplyr::select(site, yr2001, yr2013) %>% 
  rename(`2001` = 'yr2001', `2016` = 'yr2013') %>% 
  pivot_longer(names_to = 'year', values_to = 'pest', -site) %>% 
  mutate(year = as.numeric(year))

natural_pest <- percent_county_land_use %>% 
  filter(land_use_collapsed == "Natural/semi-natural") %>% 
  ungroup() %>% 
  dplyr::select(site, year, percent_county) %>% 
  left_join(pest_long)


#### run these correlations for each family because the counties modeled change by family

library(broom)

fam <- c("Andrenidae", "Apidae", "Halictidae", 
         "Megachilidae", "Colletidae|Melittidae")


nat_pest_correlation_all <- list()

nat_canag_correlation_all <- list()

net_change_canag_correlation_all <- list()

pest_change_nat_correlation_all <- list()


for(f in fam){
  
  my.data <- readRDS(paste0("clean_data/data_prepared/my_data_env_genus_filtered_trait_agriregion_both_pest_area_county_1995_2015_",f,"_ALLFALSE.rds"))
  
  ## animal pollinated agriculture + natural area
  
  nat_canag_correlation_all[[f]] <- apply(canag_percent_nat[my.data$site,1:2], 2, FUN = function(x) tidy(cor.test(canag_percent_nat[my.data$site,3], x))) %>% 
    map_df(~as.data.frame(.x), .id = 'year') %>% 
    mutate(family = f)

  net_change_canag_correlation_all[[f]] <- tidy(cor.test(canag_percent_nat[my.data$site,3], (canag_percent_nat[my.data$site,2] - canag_percent_nat[my.data$site,1]))) %>% 
    mutate(family = f)
  
  ## natural areas + pesticide 
  
  natural_pest_fam <- natural_pest %>% 
    filter(site %in% my.data$site) 
  
  nat_pest_correlation_all[[f]] <- split(natural_pest_fam, natural_pest_fam$year) %>% 
    map(~tidy(cor.test(.x$percent_county, .x$pest))) %>% 
    map_df(~as.data.frame(.x), .id = 'year') %>% 
    mutate(family = f)
  
  pest_change <- natural_pest_fam %>% 
    dplyr::select(site, year, pest) %>% 
    pivot_wider(names_from = year, values_from = pest) %>% 
    #mutate(pest_change = `2016` - `2001`) %>% 
    dplyr::select(site, `2016`)
  
  nat_change <- natural_pest_fam %>% 
    dplyr::select(site, year, percent_county) %>% 
    pivot_wider(names_from = year, values_from = percent_county) %>% 
    mutate(nat_change = `2016` - `2001`)%>% 
    dplyr::select(site, nat_change)
  
  change_together <- left_join(pest_change, nat_change) 
  
  ggplot(change_together) +
    geom_point(aes(x = `2016`, y = nat_change)) +
    ylab("change in natural areas") + xlab("Pesticide Use in 2013")
  
  pest_change_nat_correlation_all[[f]] <- tidy(cor.test(change_together$`2016`, change_together$nat_change))
  
}


nat_canag_correlation_all %>% 
  map_df(~as.data.frame(.x)) %>% 
  ggplot(aes(x = year, y = estimate, colour = family, group = family)) +
  geom_line() + 
  theme_cowplot() +
  ylab("correlation between animal pollinated ag and natural areas")

nat_pest_correlation_all %>% 
  map_df(~as.data.frame(.x)) %>% 
  ggplot(aes(x = year, y = estimate, colour = family, group = family)) +
  geom_line() +
  theme_cowplot() +
  ylab("correlation between pesticide use and natural")

