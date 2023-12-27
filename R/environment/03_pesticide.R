#### script to extract pesticide data from the USGS pesticide use program

library(dplyr)
library(data.table)
library(sf)
library(stringr)
library(tidyr)
library(stringr)

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

  ### join pesticide data with county and LD 50
  
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
  
  ### join pesticide data with county and LD 50
  
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
  

  
###### comparison of Neonics and pyrethroids with other types of insecticides ##
  
##other compounds also toxic to bees

neonic_compounds <- c("ACETAMIPRID", "CLOTHIANIDIN", "DINOTEFURAN", "IMIDACLOPRID", 
                        "THIAMETHOXAM", "THIACLOPRID")

pyr_compounds
  
carbamates <- toupper(c("Carbaryl", "Oxamyl","Carbofuran","Thiodicarb","Methomyl"))

organo_phosphates <- toupper(c("parathion", "chlorpyrifos", "diazinon", "dichlorvos", 
                       "phosmet", "fenitrothion", "tetrachlorvinphos", "azamethiphos", 
                       "azinphos-methyl", "malathion", "methyl parathion"))
  
other_toxic_compounds <- c("ABAMECTIN", "PYRIDABEN", "ACEPHATE", 
                           "CHLORETHOXYFOS",  "DIAZINON", "DIMETHOATE", 
                            "FIPRONIL", "SULFOXAFLOR")


compounds_to_check <- bind_rows(data.frame(type = 'Neonicotinoids', compound = neonic_compounds),
          data.frame(type = 'Pyrethroids', compound = pyr_compounds),
          data.frame(type = 'Carbamates', compound = carbamates),
          data.frame(type = 'Organophosphates', compound = organo_phosphates),
          data.frame(type = 'Other', compound = other_toxic_compounds))


all <- list()

for(i in 1:length(files_dir)){
  
  # read files
  pesticide_data <- fread(paste0(input_dir,files_dir[i]))
  
  ## extract neonicotinoid
  
  all[[i]] <- pesticide_data[COMPOUND %in% compounds_to_check$compound]
  
}


other_all <- rbindlist(all) %>% 
  left_join(compounds_to_check, by = c('COMPOUND' = 'compound')) 

other_all %>% 
  group_by(YEAR, type) %>% 
  summarise(total_kg = sum(EPEST_HIGH_KG)) %>% 
  ggplot(aes(x = YEAR, y = total_kg)) + geom_bar(stat = "identity", aes(fill = type))


other_all %>% 
  group_by(YEAR, type) %>% 
  summarise(total_kg = sum(EPEST_HIGH_KG)) %>% 
  ggplot(aes(x = YEAR, y = total_kg)) + geom_bar(stat = "identity", aes(fill = type), 
                                                 position = 'fill')


raw <- other_all %>% 
  filter(YEAR < 2015 & YEAR >=1994) %>% 
  group_by(YEAR, type) %>% 
  summarise(total_kg = sum(EPEST_HIGH_KG)) %>%
  ungroup() %>% 
  filter(!is.na(total_kg)) %>% 
  ggplot(aes(x = YEAR, y = total_kg)) + 
  geom_point(aes(colour = type)) +
  geom_line(aes(colour = type)) +
  ylab('Total Kg used') +
  xlab('Year') + 
  theme_cowplot() +labs(colour="Type of insecticide")+
  theme(legend.position = 'bottom')+
  scale_x_continuous(limits = c(1994, 2015.5))

log_t <- other_all %>% 
  filter(YEAR < 2015 & YEAR >=1994) %>% 
  group_by(YEAR, type) %>% 
  summarise(total_kg = sum(EPEST_HIGH_KG)) %>%
  ungroup() %>% 
  filter(!is.na(total_kg)) %>% 
  ggplot(aes(x = YEAR, y = total_kg)) + 
  geom_point(aes(colour = type)) +
  geom_line(aes(colour = type)) +
  ylab('Total Kg used (log)') +
  xlab('Year') + 
  theme_cowplot() +labs(colour=" ")+
  scale_y_log10()+
  theme(legend.position = 'none') +
  scale_x_continuous(limits = c(1994, 2015.5))

other_pesticided <- plot_grid(raw, log_t, align = 'hv', axis = "bt", labels =c("A.", "B."))

ggsave(other_pesticided, filename= 'plots/other_pesticides.pdf', width = 12)


### most used compound ##

other_all %>% 
  filter(YEAR < 2015 & YEAR >=1994) %>% 
  group_by(COMPOUND, type) %>% 
  summarise(total_tonne = sum(EPEST_HIGH_KG, na.rm = TRUE)*0.001) %>% 
  group_by(type) %>% 
  arrange(desc(total_tonne)) %>% 
  slice(1:2) %>% View()
