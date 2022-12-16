library(dplyr)
library(data.table)
library(sf)
library(stringr)
library(tidyr)
library(stringr)

##### new ####

## download county level pesticide data from https://water.usgs.gov/nawqa/pnsp/usage/maps/county-level/


input_dir <- "/Volumes/Rasters/USC/bee_occupancy/raw_data/pesticide/pesticide_US/"
files_dir <- list.files(input_dir)

## create list of pesticides of interest -- compound names from above website

neonics <- list()

neonic_compounds <- c("ACETAMIPRID", "CLOTHIANIDIN", "DINOTEFURAN", "IMIDACLOPRID", 
                      "THIAMETHOXAM", "THIACLOPRID")


### for each year extract compounds of interest 

for(i in 1:length(files_dir)){
  
  # read files
  pesticide_data <- fread(paste0(input_dir,files_dir[i]))
  
  ## extract neonicotinoid
  
  neonics[[i]] <- pesticide_data[COMPOUND %in% neonic_compounds]
  
}

neonics_all <- rbindlist(neonics)

saveRDS(neonics_all, "clean_data/pesticide/neonics_county.rds")

######## add pesticide data to sites ####

## get sites

sites <- readRDS("clean_data/sites/sites_counties.RDS")

## Get LD50 data from ECOTOX database

ld50 <- read.csv("clean_data/pesticide/apis_ld50_mean.csv")

ld50_dermal <- ld50 %>% 
  mutate(COMPOUND = toupper(Chemical_short)) %>% 
  filter(Exposure_Type == 'Dermal') %>% 
  dplyr::select(COMPOUND, mean_ld50)

## load pesticide data
neonics_all <- readRDS("clean_data/pesticide/neonics_county.rds")

  ## neonics
  
  neonic_all_arranged <- neonics_all %>% 
    mutate(STATEFP = str_pad(STATE_FIPS_CODE, 2, "left", 0), 
           COUNTYFP = str_pad(COUNTY_FIPS_CODE, 2, "left", 0)) %>% 
    dplyr::select(YEAR, STATEFP, COUNTYFP, EPEST_HIGH_KG, COMPOUND) %>% 
    unite("state_county", STATEFP, COUNTYFP)

  
  neonic_ld50 <- neonic_all_arranged %>% 
    mutate(EPEST_HIGH_KG = ifelse(is.na(EPEST_HIGH_KG), 0, EPEST_HIGH_KG)) %>% 
    group_by(state_county, COMPOUND, YEAR) %>% 
    summarise(pest_site = sum(EPEST_HIGH_KG)) %>% 
    left_join(ld50_dermal) %>% 
    mutate(pest_site_ld50 = pest_site/mean_ld50)
  
  saveRDS(neonic_ld50, file = paste0("clean_data/pesticide/neonics_US_county.rds"))
  
