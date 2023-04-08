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
           COUNTYFP = str_pad(COUNTY_FIPS_CODE, 3, "left", 0)) %>% 
    dplyr::select(YEAR, STATEFP, COUNTYFP, EPEST_HIGH_KG, COMPOUND) %>% 
    unite("state_county", STATEFP, COUNTYFP)
  
  neonic_ld50 <- neonic_all_arranged %>% 
    mutate(EPEST_HIGH_KG = ifelse(is.na(EPEST_HIGH_KG), 0, EPEST_HIGH_KG)) %>% 
    group_by(state_county, COMPOUND, YEAR) %>% 
    summarise(pest_site = sum(EPEST_HIGH_KG)) %>% 
    left_join(ld50_dermal) %>% 
    mutate(pest_site_ld50 = pest_site/mean_ld50)
  
  saveRDS(neonic_ld50, file = paste0("clean_data/pesticide/neonics_US_county.rds"))
  

  
  
  
  
  
  #####  pyrethroids  ####
  
  ## download county level pesticide data from https://water.usgs.gov/nawqa/pnsp/usage/maps/county-level/
  
  
  input_dir <- "/Volumes/Rasters/USC/bee_occupancy/raw_data/pesticide/pesticide_US/"
  files_dir <- list.files(input_dir)
  
  ld50 <- read.csv("clean_data/pesticide/apis_ld50_mean_pyrethoid.csv")
  
  
  
  ## create list of pesticides of interest -- compound names from above website
  
  pyr <- list()
  
  pyr_compounds <- toupper(unique(ld50$Compound)) 
  
  pyr_compounds[pyr_compounds == "FLUVALINATE-TAU"] <- "FLUVALINATE TAU"
  
  ### for each year extract compounds of interest 
  
  for(i in 1:length(files_dir)){
    
    # read files
    pesticide_data <- fread(paste0(input_dir,files_dir[i]))
    
    ## extract pyrethroids
    
    pyr[[i]] <- pesticide_data[COMPOUND %in% pyr_compounds]
    
  }
  
  pyr_all <- rbindlist(pyr)
  
  saveRDS(pyr_all, "clean_data/pesticide/pyr_county.rds")
  
  ######## add pesticide data to sites ####
  
  ## get sites
  
  sites <- readRDS("clean_data/sites/sites_counties.RDS")
  
  ## Get LD50 data from ECOTOX database
  
  ld50_dermal <- ld50 %>% 
    mutate(COMPOUND = toupper(Compound)) %>% 
    mutate(COMPOUND = ifelse(COMPOUND == "FLUVALINATE-TAU", "FLUVALINATE TAU", COMPOUND)) %>% 
    filter(Exposure_Type_simple == 'Dermal') %>% 
    dplyr::select(COMPOUND, mean_ld50)
  
  ## load pesticide data
  pyr_all <- readRDS("clean_data/pesticide/pyr_county.rds")
  
  ## pyr
  
  pyr_all_arranged <- pyr_all %>% 
    mutate(STATEFP = str_pad(STATE_FIPS_CODE, 2, "left", 0), 
           COUNTYFP = str_pad(COUNTY_FIPS_CODE, 3, "left", 0)) %>% 
    dplyr::select(YEAR, STATEFP, COUNTYFP, EPEST_HIGH_KG, COMPOUND) %>% 
    unite("state_county", STATEFP, COUNTYFP)
  
  
  pyr_ld50 <- pyr_all_arranged %>% 
    mutate(EPEST_HIGH_KG = ifelse(is.na(EPEST_HIGH_KG), 0, EPEST_HIGH_KG)) %>% 
    group_by(state_county, COMPOUND, YEAR) %>% 
    summarise(pest_site = sum(EPEST_HIGH_KG)) %>% 
    left_join(ld50_dermal) %>% 
    mutate(pest_site_ld50 = pest_site/mean_ld50)
  
  saveRDS(pyr_ld50, file = paste0("clean_data/pesticide/pyr_US_county.rds"))
  
  
  
#### all pesticides california 
  
california_pest <- list()
  for(i in 1:length(files_dir)){
    
    # read files
    pesticide_data <- fread(paste0(input_dir,files_dir[i]))
    
    ## extract neonicotinoid
    
    california_pest[[i]] <- pesticide_data[STATE_FIPS_CODE %in% "6"]
    
  }

all_cal_pest <- california_pest %>% 
  rbindlist()


all_cal_pest$COUNTY_FIPS_CODE %>% unique()


## from environment
environment <- readRDS("clean_data/data_prepared/environment_counties_1995_2015.rds")

california_env <- environment$neonic_mat[str_detect(rownames(environment$neonic_mat), "s_06"),]

names(which(apply(california_env != min(california_env), 1, any)))

# california counties with neonics
#s_06_101 s_06_103 s_06_105 s_06_107 s_06_109 s_06_111 s_06_113 s_06_115 
c(101, 103, 105, 107, 109, 111, 113, 115)

neonics_all <- readRDS("clean_data/pesticide/neonics_county.rds")

unique(neonics_all[STATE_FIPS_CODE == '6' & YEAR %in% 1995:2015, COUNTY_FIPS_CODE]) %>% sort()
[1]   1   5   7   9  11  13  15  17  19  21  23  25  29  31  33  35  37  39  41  43  45  47  49  51  53  55  57  59
[29]  61  65  67  69  71  73  75  77  79  81  83  85  87  89  93  95  97  99 101 103 105 107 109 111 113 115
