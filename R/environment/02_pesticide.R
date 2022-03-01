library(dplyr)
library(data.table)
library(sf)
library(rgeos)
library(ggplot2)
library(cowplot)
library(tidyr)

### pesticides of interest for bees 
#"IMIDACLOPRID" 
#Osmia  https://www.pnas.org/content/118/48/e2109909118.full#ref-37
# Apis M https://link.springer.com/article/10.1007/s10646-010-0566-0 (meta analysis)
# Bombus Terrestris https://science.sciencemag.org/content/336/6079/351.abstract 
# Bombus Terrestris https://www.nature.com/articles/nature11585
#"THIAMETHOXAM"
#Apis M  https://www.science.org/doi/abs/10.1126/science.1215039

### EPA
## LD50 for highly toxic for honey bees https://www.mda.state.mn.us/protecting/bmps/pollinators/beetoxicity
## neonicotinoids as a whole category

#### highly toxic for honey bees
# ABAMECTIN, CARBARYL, OXAMYL, PYRIDABEN, ACEPHATE, CHLORETHOXYFOS, CHLORPYRIFOS, DIAZINON, DIMETHOATE, MALATHION, FIPRONIL, PYRETHRIN, SULFOXAFLOR

##### Pyrethroid
## ALPHA-CYPERMETHRIN, BETA-CYFLUTHRIN, BIFENTHRIN, CYFLUTHRIN, DELTAMETHRIN, ESFENVALERATE, GAMMA-CYHALOTHRIN, LAMBDA-CYHALOTHRIN, PERMETHRIN, TEFLUTHRIN, ZETA-CYPERMETHRIN

##### neonicotinoids
# ACETAMIPRID, CLOTHIANIDIN, DINOTEFURAN, IMIDACLOPRID, THIAMETHOXAM




## download county level pesticide data from https://water.usgs.gov/nawqa/pnsp/usage/maps/county-level/


input_dir <- "/Volumes/Rasters/USC/bee_occupancy/raw_data/pesticide/pesticide_US/"
files_dir <- list.files(input_dir)

## create list of pesticides of interest -- compound names from above website

gen_toxic <- list()

gen_toxic_compounds <- c("ABAMECTIN", "CARBARYL", "OXAMYL", "PYRIDABEN", "ACEPHATE", 
                         "CHLORETHOXYFOS", "CHLORPYRIFOS", "DIAZINON", "DIMETHOATE", 
                         "MALATHION", "FIPRONIL", "SULFOXAFLOR")

pyrethroid <- list()

pyrethroid_compunds <- c("ALPHA CYPERMETHRIN", "BIFENTHRIN", "CYFLUTHRIN", 
                         "DELTAMETHRIN", "ESFENVALERATE", "CYHALOTHRIN-GAMMA", "CYHALOTHRIN-LAMBDA", 
                         "PERMETHRIN", "TEFLUTHRIN", "ZETA-CYPERMETHRIN")

neonics <- list()

neonic_compounds <- c("ACETAMIPRID", "CLOTHIANIDIN", "DINOTEFURAN", "IMIDACLOPRID", "THIAMETHOXAM")


### for each year extract compounds of interest 

for(i in 1:length(files_dir)){
  
  # read files
  pesticide_data <- fread(paste0(input_dir,files_dir[i]))
  
  ## extract generic organophospahes
  
  gen_toxic[[i]] <- pesticide_data[COMPOUND %in% gen_toxic_compounds]
  
  ## extract pyrethroid
  
  pyrethroid[[i]] <- pesticide_data[COMPOUND %in% pyrethroid_compunds]
  
  ## extract neonicotinoid
  
  neonics[[i]] <- pesticide_data[COMPOUND %in% neonic_compounds]
  
}

gen_toxic_all <- rbindlist(gen_toxic)

pyrethroid_all <- rbindlist(pyrethroid)

neonics_all <- rbindlist(neonics)


saveRDS(gen_toxic_all, "clean_data/pesticide/gen_toxic_county.rds")
saveRDS(pyrethroid_all, "clean_data/pesticide/pyrethroid_county.rds")
saveRDS(neonics_all, "clean_data/pesticide/neonics_county.rds")





######## add pesticide data to sites ####

## pesticide data only for US

### county map data ##

### download county map from https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html

# Load shapefile

us_counties <- read_sf('/Volumes/Rasters/USC/bee_occupancy/raw_data/pesticide/cb_2018_us_county_20m/cb_2018_us_county_20m.shp')

## select only county and state codes

state_county_fp <- us_counties[,c("STATEFP", "COUNTYFP")]

spdf <- as_Spatial(state_county_fp)


## load pesticide data
neonics_all <- readRDS("clean_data/pesticide/neonics_county.rds")
gen_toxic_all <- readRDS("clean_data/pesticide/gen_toxic_county.rds")
pyrethroid_all <- readRDS("clean_data/pesticide/pyrethroid_county.rds")


pest_sites <- function(resolution){
  
  ## load sites
  
  sites <- readRDS(paste0("clean_data/sites/sites_US_",resolution, ".rds"))
  
  ## reproject sites to county crs
  
  prj3<-"+proj=longlat +datum=NAD83 +no_defs"
  
  sites_t <- spTransform(sites, CRS(prj3))
  
  ## find the counties per site
  
  sites_per_county <- over(spdf, sites_t)
  
  site_county <- data.frame(STATE_FIPS_CODE = as.integer(state_county_fp$STATEFP), COUNTY_FIPS_CODE = as.integer(state_county_fp$COUNTYFP),
                            site = sites_per_county) %>% 
    data.table()
  
  ## set joining keys
  setkeyv(site_county, c("STATE_FIPS_CODE", "COUNTY_FIPS_CODE"))
  setkeyv(gen_toxic_all, c("STATE_FIPS_CODE", "COUNTY_FIPS_CODE"))
  setkeyv(pyrethroid_all, c("STATE_FIPS_CODE", "COUNTY_FIPS_CODE"))
  setkeyv(neonics_all, c("STATE_FIPS_CODE", "COUNTY_FIPS_CODE"))
  
  
  # perform the join, eliminating not matched rows from Right
  
  names_df <- c("year", "site", "compound", "epest_high", "epest_low")
  
  ## organophosphates
  
  gen_toxic_site <- gen_toxic_all[site_county][!is.na(COMPOUND) & !is.na(site)]

  gen_toxic_site_year <- gen_toxic_site[, .(mean(EPEST_HIGH_KG, na.rm = TRUE), mean(EPEST_LOW_KG, na.rm = TRUE)), by = .(YEAR, site, COMPOUND)]

  colnames(gen_toxic_site_year) <- names_df
  
  saveRDS(gen_toxic_site_year, file = paste0("clean_data/pesticide/gen_toxic_US_",resolution, ".rds"))
  
  
  ### pyrethroid
  
  pyrethroid_site <- pyrethroid_all[site_county][!is.na(COMPOUND) & !is.na(site)]

  pyrethroid_site_year <- pyrethroid_site[, .(mean(EPEST_HIGH_KG, na.rm = TRUE), mean(EPEST_LOW_KG, na.rm = TRUE)), by = .(YEAR, site, COMPOUND)]

  colnames(pyrethroid_site_year) <- names_df
  
  saveRDS(pyrethroid_site_year, file = paste0("clean_data/pesticide/pyrethroid_US_",resolution, ".rds"))

  ## neonics
  
  neonics_site <- neonics_all[site_county][!is.na(COMPOUND) & !is.na(site)]

  neonics_site_year <- neonics_site[, .(mean(EPEST_HIGH_KG, na.rm = TRUE), mean(EPEST_LOW_KG, na.rm = TRUE)), by = .(YEAR, site, COMPOUND)]

  colnames(neonics_site_year) <- names_df
  
  saveRDS(neonics_site_year, file = paste0("clean_data/pesticide/neonics_US_",resolution, ".rds"))
  
}


### pesticide only for the US no option for country

## assign sites for 100km resolution

pest_sites(100)

## assign sites for 50km resolution

pest_sites(50)




