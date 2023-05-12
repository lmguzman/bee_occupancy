library(sf)
library(dplyr)
library(raster)
library(exactextractr)
library(data.table)
library(stringr)
library(ggplot2)
library(purrr)

region_df <- readRDS("clean_data/sites/site_counties_agriregion.rds") %>% 
  mutate(region_collapsed = case_when(region %in% c("Southern Seaboard", "Eastern Uplands",
                                                    "Mississippi Portal") ~ "South East",
                                      region %in% c("Heartland", "Prairie Gateway") ~ "Central",
                                      TRUE ~ region)) %>%
  mutate(site = paste0("s_", state_county))


sites <- readRDS("clean_data/sites/sites_counties.RDS")

area <- readRDS("clean_data/sites/area_counties.RDS")

year <- 2008
  
## load crop data
crop_cover <- raster(paste0('raw_data/crops/', year, '_30m_cdls/',year,'_30m_cdls.tif'))
  
plot(crop_cover)

## extract all of the crop types in the each county

for(i in 1:nrow(sites)){
  crop_cover_site <- extract(crop_cover, sites[i,])
  saveRDS(crop_cover_site, paste0('clean_data/agriculture/crop_cover_all/crop_county',sites[i,]$state_county,'.rds'))
}


## load crop data and aggregate

county_crop <- list.files('clean_data/agriculture/crop_cover_all/', full.names = TRUE)

all_crop_cover <- list()
counter <- 1
for(crop_file in county_crop){

  county <- str_extract(crop_file, "\\d+\\_\\d+")
  crop_cover_site <- readRDS(crop_file)
  all_crop_cover[[counter]] <- data.frame(table(crop_cover_site), state_county = county)
 counter <- counter+1
 print(counter)
}


agriculture_all_year <-rbindlist(all_crop_cover)

saveRDS(agriculture_all_year, 'clean_data/agriculture/crops_county.rds')
 
 ### add categorization codes

agriculture_all_year <- readRDS('clean_data/agriculture/crops_county.rds')

crop_categories <- read.csv("raw_data/crops/Categorization_Code_Land_Cover.txt") %>%
  mutate(Categorization_Code = as.numeric(Categorization_Code)) %>%
  mutate(cover_type = case_when(Categorization_Code == 0 ~ "Background", 
                                Categorization_Code %in% 1:60  ~ "Crop",
                                Categorization_Code %in% 61:65  ~ "Non Crop",
                                Categorization_Code %in% 66:80  ~ "Crop",
                                Categorization_Code %in% 81:109  ~ "Other",
                                Categorization_Code %in% 110:195  ~ "NLCD",
                                Categorization_Code %in% 195:255  ~ "Crop")) %>% 
  mutate(non_animal_pollinated = str_detect(Land_Cover, "Corn|Wheat|Wht|Rice|Soybean|Sorghum|Barley|Oat")) %>% 
  mutate(non_animal_pollinated = ifelse(non_animal_pollinated == TRUE & str_detect(Land_Cover, "Cotton|Lettuce"), "HALF", non_animal_pollinated))

region_agriculture_all <- agriculture_all_year %>%
  mutate(crop_cover_site = as.numeric(as.character(crop_cover_site))) %>%
  left_join(crop_categories, by = c("crop_cover_site" = "Categorization_Code")) %>%
  left_join(region_df) %>%
  mutate(site = paste0("s_", state_county)) %>% 
  dplyr::select(Land_Cover, Freq, region_collapsed, site, cover_type, crop_cover_site, non_animal_pollinated)

saveRDS(region_agriculture_all, 'clean_data/agriculture/crops_county_animal.rds')



## crop richness in each county 
region_agriculture_all %>%
  filter(cover_type == 'Crop') %>%
  count(site, Land_Cover) %>%
  count(site)

## crop richness in each region

region_agriculture_all %>%
  filter(cover_type == 'Crop') %>%
  count(region_collapsed, Land_Cover)

region_agriculture_all %>%
  filter(cover_type == 'Crop') %>%
  count(site) %>%
  left_join(region_df) %>% 
  ggplot(aes(x = region_collapsed, y = n)) +
  geom_boxplot() +
  ylab('Distribution of number of crops per county')

region_agriculture_all %>%
  filter(cover_type == 'Crop') %>%
  count(region_collapsed, Land_Cover) %>%
  count(region_collapsed)


## crop diversity

library(vegan)

region_agriculture_all %>%
  filter(cover_type == 'Crop') %>%
  group_by(site, region_collapsed) %>%
  summarise(Shannon = diversity(Freq, index = 'shannon')) %>%
  group_by(region_collapsed) %>%
  summarise(mean(Shannon))

## plot crop diversity

region_agriculture_all %>%
  filter(cover_type == 'Crop') %>%
  group_by(site, region_collapsed) %>%
  summarise(Shannon = diversity(Freq, index = 'shannon')) %>%
  ggplot(aes(x = region_collapsed, y = Shannon)) +
  geom_boxplot() 


## crop evenness

region_agriculture_all %>%
  filter(cover_type == 'Crop') %>%
  group_by(site, region_collapsed) %>%
  summarise(evenness = diversity(Freq, index = 'invsimpson')) %>%
  group_by(region_collapsed) %>%
  summarise(mean(evenness))

### percentage of crops that is not animal pollinated

##corn, wheat, rice, soybean and sorghum

animal_pol <- region_agriculture_all %>%
  filter(cover_type == 'Crop') %>% 
  mutate(animal_pollinated = str_detect(Land_Cover, "Corn|Wheat|Wht|Rice|Soybean|Sorghum")) %>% 
  group_by(site, animal_pollinated) %>% 
  summarise(freq_an = sum(Freq))
  

total <- region_agriculture_all %>%
  filter(cover_type == 'Crop') %>% 
  mutate(animal_pollinated = str_detect(Land_Cover, "Corn|Wheat|Wht|Rice|Soybean|Sorghum")) %>% 
  group_by(site) %>% 
  summarise(freq_all = sum(Freq))

saveRDS(total, "clean_data/agriculture/crop_cover_county.rds")

animal_polinated_region <- animal_pol %>% 
  left_join(total) %>% 
  mutate(percentage = freq_an/freq_all) %>% 
  dplyr::select(site, animal_pollinated, percentage) %>% 
  filter(animal_pollinated == FALSE) %>% 
  left_join(region_df)
  

animal_polinated_region %>% 
  ggplot(aes(x = region_collapsed, y = percentage)) +
  geom_boxplot() +
  ylab('Percentage of crops that are animal pollinated')


library(emmeans)
my.mod      <- glm(percentage~region_collapsed, data=animal_polinated_region, 
                   family=binomial(link = 'logit'))
summary(my.mod)
em <- emmeans(my.mod, "region_collapsed")
contrast(em, "pairwise", adjust = "Tukey")

### crop richness for animal pollinated crops

crop_richness_animal_pol <- region_agriculture_all %>%
  filter(cover_type == 'Crop') %>% 
  mutate(non_animal_pollinated = str_detect(Land_Cover, "Corn|Wheat|Wht|Rice|Soybean|Sorghum")) %>% 
  filter(non_animal_pollinated == FALSE) %>%
  count(site) %>%
  left_join(region_df) 

crop_richness_animal_pol %>% 
  ggplot(aes(x = region_collapsed, y = n)) +
  geom_boxplot() +
  ylab('Distribution of number of crops per county')


library(emmeans)
my.mod      <- glm(n~region_collapsed, data=crop_richness_animal_pol, 
                   family=poisson(link = 'log'))
summary(my.mod)
em <- emmeans(my.mod, "region_collapsed")
contrast(em, "pairwise", adjust = "Tukey")



region_agriculture_all <- readRDS('clean_data/agriculture/crops_county_animal.rds')

Total_county_area <- region_agriculture_all %>% 
  group_by(site) %>% 
  summarise(total_area = sum(Freq))

Total_animal_pollinated <- region_agriculture_all %>% 
  filter(cover_type == 'Crop') %>% 
  filter(non_animal_pollinated %in% c("FALSE", "HALF")) %>% 
  group_by(site) %>% 
  summarise(total_ani_pollinated = sum(Freq))

frac_animal_pollinated_all <- Total_county_area %>% 
  left_join(Total_animal_pollinated) %>% 
  mutate(county_animal_pol = total_ani_pollinated/total_area)

county_animal_all <- frac_animal_pollinated_all %>% 
  dplyr::select(site, county_animal_pol) %>% 
  mutate(county_animal_log = log(county_animal_pol + 0.000001)) 





county_animal_all


region_df <- c("Fruitful_Rim", "Central", "Northern_Great_Plains", "Basin_and_Range", "Northern_Crescent", "South_East")

modelled_sites <- list()
  
for(region in region_df){
  
  my.data <- readRDS(paste0("clean_data/data_prepared/my_data_env_genus_filtered_trait_agriregion_both_pest_area_1995_2015_ALL_",region,"FALSE.rds"))

  modelled_sites[[region]] <- data.frame(site = my.data[[4]], region = region)
}



county_animal_all_region <- county_animal_all %>% 
  left_join(map_df(modelled_sites, ~as.data.frame(.x))) %>% 
  filter(!is.na(region))

county_animal_all_region %>% 
  ggplot(aes(x = county_animal_log)) +
  geom_histogram() +
  facet_wrap(~region, scales = 'free_y') +
  theme_cowplot()

county_animal_all_region %>% 
  ggplot(aes(x = county_animal_pol)) +
  geom_histogram() +
  facet_wrap(~region, scales = 'free_y') +
  theme_cowplot()


region_modelled <- region_agriculture_all %>% 
  left_join(map_df(modelled_sites, ~as.data.frame(.x))) %>% 
  filter(!is.na(region)) %>% 
  filter(cover_type == 'Crop') %>% 
  mutate(land_cover_new = case_when(str_detect(Land_Cover, "Corn|Soy|Cotton") ~ "GM",
                                    TRUE ~ "Other"))


ind_crop_region <- region_modelled %>% 
  group_by(region, land_cover_new) %>% 
  summarise(ind_crop = sum(Freq))

total_crop_region <- region_modelled %>% 
  group_by(region) %>% 
  summarise(total_crop = sum(Freq))

ind_crop_percent <- ind_crop_region %>% 
  left_join(total_crop_region) %>% 
  mutate(percent_crop_region = ind_crop/total_crop) 

ind_crop_percent %>% 
  ggplot(aes(x = region, y = percent_crop_region, fill = land_cover_new)) +
  geom_bar(stat = 'identity')
