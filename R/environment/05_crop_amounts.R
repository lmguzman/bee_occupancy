## script to extract crop type from the crop data layer

library(sf)
library(dplyr)
library(raster)
library(data.table)
library(stringr)
library(ggplot2)
library(purrr)
library(tidyr)

## load sites 

sites <- readRDS("clean_data/sites/sites_counties.RDS")

year <- 2008
  
## load crop data
crop_cover <- raster(paste0('raw_data/crops/', year, '_30m_cdls/',year,'_30m_cdls.tif'))
  
## extract all of the crop types in the each county and save per county

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
 
######### process data by adding category types ####

## re-load compiled agricultural data

agriculture_all_year <- readRDS('clean_data/agriculture/crops_county.rds')

## load and clean the pollinator attractiveness data from the USDA

pol_attc_land_use <- read.csv("raw_data/crops/pollinator_attractiveness 2.csv")

pol_attractiveness <- read.csv("raw_data/crops/Copy of pollinator_attractiveness.csv") %>% 
  dplyr::select(-c(X.1, X.6, X.8, X.11, X.12, X.14, X.15, X.18:X.27)) %>% 
  unite(Crop, X, "Crop", sep = "") %>% mutate(Crop = str_trim(Crop)) %>% 
  unite(Description, X.2, X.3, "Description", sep = "") %>% mutate(Description = str_trim(Description)) %>% 
  unite(HB.Poll.1, X.4, "HB.Poll.1", sep = "") %>% mutate(HB.Poll.1 = str_trim(HB.Poll.1)) %>% 
  unite(HB.Nec.1, X.5, "HB.Nec.1", sep = "") %>% mutate(HB.Nec.1 = str_trim(HB.Nec.1)) %>% 
  unite(Bumble.Bees, X.7, "Bumble.Bees", sep = "") %>% mutate(Bumble.Bees = str_trim(Bumble.Bees)) %>% 
  unite(Solitary.Bees,  X.9, X.10, "Solitary.Bees", sep = "") %>% mutate(Solitary.Bees = str_trim(Solitary.Bees)) %>% 
  mutate(Requires.Bee.Pollination = ifelse(Requires.Bee.Pollination == "", Uses.Managed.Pollinators, Requires.Bee.Pollination)) %>% 
  mutate(Requires.Bee.Pollination = ifelse(Requires.Bee.Pollination == "", `X.13`, Requires.Bee.Pollination)) %>% 
  mutate(Uses.Managed.Pollinators = ifelse(X.16 != "", X.16, Uses.Managed.Pollinators)) %>% 
  mutate(Uses.Managed.Pollinators = ifelse(X.17 != "", X.17, Uses.Managed.Pollinators)) %>% 
  dplyr::select(-c(X.13:X.17)) 

## get crops that do not use managed bees

Crops_not_using_managed_bees <- pol_attractiveness %>% 
  left_join(pol_attc_land_use) %>% 
  filter(Uses.Managed.Pollinators == 'No') %>% 
  dplyr::select(Crop, Land_Cover)

## get crops not attractive to bumbles and solitary

Crops_not_attractive_bb_sol <- pol_attractiveness %>% 
  left_join(pol_attc_land_use) %>% 
  filter(Bumble.Bees == "-" | Solitary.Bees == '-') %>% 
  dplyr::select(Crop, Land_Cover)

## assign crop numbers to category names and join with the attractiveness

crop_categories <- read.csv("raw_data/crops/Categorization_Code_Land_Cover.txt") %>%
  mutate(Categorization_Code = as.numeric(Categorization_Code)) %>% 
  mutate(cover_type = case_when(Categorization_Code == 0 ~ "Background", ### using the original categorization
                                Categorization_Code %in% 1:60  ~ "Crop",
                                Categorization_Code %in% 61:65  ~ "Non Crop",
                                Categorization_Code %in% 66:80  ~ "Crop",
                                Categorization_Code %in% 81:109  ~ "Other",
                                Categorization_Code %in% 110:195  ~ "NLCD",
                                Categorization_Code %in% 195:255  ~ "Crop")) %>% 
  ## general appraoch for non-animal pollinated crops
  mutate(non_animal_pollinated_orig = str_detect(Land_Cover, "Corn|Wheat|Wht|Rice|Soybean|Sorghum|Barley|Oat")) %>% 
  mutate(non_animal_pollinated_orig = ifelse(non_animal_pollinated_orig == TRUE & str_detect(Land_Cover, "Cotton|Lettuce"), "HALF", non_animal_pollinated_orig)) %>% 
  ### using the crops that do not use managed pollinators 
  mutate(crops_do_not_use_managed_bees = ifelse(Land_Cover %in% Crops_not_using_managed_bees$Land_Cover, TRUE, FALSE)) %>%
  mutate(crops_do_not_use_managed_bees = ifelse(str_detect(Land_Cover, "Dbl Crop Lettuce/Cantaloupe"), "HALF", crops_do_not_use_managed_bees)) %>% 
  ### crops that do not require pollination 
  mutate(crops_not_require_pol = ifelse(Land_Cover %in% Crops_not_requiring_pollination$Land_Cover, TRUE, FALSE)) %>% 
  mutate(crops_not_require_pol = ifelse(str_detect(Land_Cover, "Dbl Crop Lettuce/Cantaloupe"), "HALF", crops_not_require_pol))  %>% 
  ### crops that are not attractive to all pollinator categories 
  mutate(crops_not_attractive_all = ifelse(Land_Cover %in% Crops_not_attractive_all$Land_Cover, TRUE, FALSE)) %>% 
  mutate(crops_not_attractive_all = ifelse(Land_Cover %in% c("Dbl Crop WinWht/Soybeans", "Dbl Crop Lettuce/Durum Wht", "Dbl Crop Barley/Sorghum",
                                                             "Dbl Crop WinWht/Corn", "Dbl Crop Lettuce/Barley", "Dbl Crop WinWht/Sorghum", 
                                                             "Dbl Crop Oats/Corn", "Dbl Crop Durum Wht/Sorghum", "Dbl Crop Barley/Corn",
                                                             "Dbl Crop WinWht/Cotton", "Dbl Crop Soybeans/Oats", "Dbl Crop Barley/Soybeans",
                                                             "Dbl Crop Triticale/Corn"), "HALF", crops_not_attractive_all)) %>% 
  ### crops that are not attractive to bbees or solitary categories 
  mutate(crops_not_attractive_bb_sol = ifelse(Land_Cover %in% Crops_not_attractive_bb_sol$Land_Cover, TRUE, FALSE)) %>% 
  mutate(crops_not_attractive_bb_sol = ifelse(Land_Cover %in% c("Dbl Crop WinWht/Soybeans", "Dbl Crop WinWht/Corn", "Dbl Crop Oats/Corn",
                                                             "Dbl Crop Triticale/Corn", "Dbl Crop Lettuce/Durum Wht", "Dbl Crop Lettuce/Barley",
                                                             "Dbl Crop Durum Wht/Sorghum", "Dbl Crop Barley/Sorghum", "Dbl Crop WinWht/Sorghum",
                                                             "Dbl Crop Barley/Corn", "Dbl Crop WinWht/Cotton",
                                                             "Dbl Crop Soybeans/Oats", "Dbl Crop Barley/Soybeans"), "HALF", crops_not_attractive_bb_sol))
  
  

## join with the county land cover

region_agriculture_all <- agriculture_all_year %>%
  mutate(crop_cover_site = as.numeric(as.character(crop_cover_site))) %>%
  left_join(crop_categories, by = c("crop_cover_site" = "Categorization_Code")) %>%
  mutate(site = paste0("s_", state_county)) %>% 
  dplyr::select(Land_Cover, Freq, site, cover_type, crop_cover_site, non_animal_pollinated_orig:crops_not_attractive_bb_sol)

saveRDS(region_agriculture_all, 'clean_data/agriculture/crops_county_animal.rds')


