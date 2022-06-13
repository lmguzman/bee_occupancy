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

cont_usa <- state_county_fp %>% 
  filter(!STATEFP %in% c("02", "15", "60", "66", "69", "72", "78"))


## Get LD50 data

### harvest LD50s

library(rvest)

LD_50_mn<- read_html("https://www.mda.state.mn.us/protecting/bmps/pollinators/beetoxicity") %>% 
  html_table(fill = TRUE)

LD_50_mn <- LD_50_mn[[1]]

# fix colnames
colnames(LD_50_mn) <- str_replace(str_remove_all(str_replace_all(colnames(LD_50_mn), " ", "_"), "\\(|\\)"), "\\/", "_")

#remove commas and stars, make numeric
LD_50_clean <- LD_50_mn %>% 
  mutate(Honey_Bee_Contact_LD_50_ug_bee = as.numeric(str_remove_all(str_replace_all(Honey_Bee_Contact_LD_50_ug_bee, ",", ""), "\\*")),
         Honey_Bee_Contact_LD_50_ppb = as.numeric(str_remove_all(str_replace_all(Honey_Bee_Contact_LD_50_ppb, ",", ""), "\\*")),
         Honey_Bee_Oral_LD_50_ug_bee = as.numeric(str_remove_all(str_replace_all(Honey_Bee_Oral_LD_50_ug_bee, ",", ""), "\\*")),
         Honey_Bee_Oral_LD_50_ppb = as.numeric(str_remove_all(str_replace_all(Honey_Bee_Oral_LD_50_ppb, ",", ""), "\\*")))


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
  
  county_site_interception <- st_intersection(st_as_sf(spdf), st_as_sf(sites_t))
  
  ## calculate area for each county
  
  area_polygon <- st_area(county_site_interception)
  
  # area of county that intersects site
  county_site_area <- data.frame(STATEFP = county_site_interception$STATEFP, COUNTYFP = county_site_interception$COUNTYFP, site = county_site_interception$site, area_poly = as.numeric(area_polygon))
  
  ## total area of county
  area_county <- st_area(state_county_fp)
  
  county_area <- data.frame(STATEFP = state_county_fp$STATEFP, COUNTYFP = state_county_fp$COUNTYFP, area_county = as.numeric(area_county))
  
  
  ## neonics
  
  neonic_all_arranged <- neonics_all %>% 
    mutate(STATEFP = str_pad(STATE_FIPS_CODE, 2, "left", 0), 
           COUNTYFP = str_pad(COUNTY_FIPS_CODE, 2, "left", 0)) %>% 
    dplyr::select(YEAR, STATEFP, COUNTYFP, EPEST_HIGH_KG, COMPOUND) 
  
  neonic_ld50 <- county_site_area %>% 
    left_join(county_area) %>% 
    arrange(STATEFP, COUNTYFP) %>% 
    left_join(neonic_all_arranged) %>% 
    mutate(EPEST_HIGH_KG = ifelse(is.na(EPEST_HIGH_KG), 0, EPEST_HIGH_KG)) %>% 
    mutate(pest_area = (EPEST_HIGH_KG*area_poly)/area_county) %>% 
    group_by(site, COMPOUND, YEAR) %>% 
    summarise(pest_site = sum(pest_area)) %>% 
    left_join(dplyr::select(LD_50_clean, Active_Ingredient, Honey_Bee_Contact_LD_50_ug_bee), by = c("COMPOUND" = "Active_Ingredient")) %>% 
    mutate(pest_site_ld50 = pest_site/Honey_Bee_Contact_LD_50_ug_bee)
  
  saveRDS(neonic_ld50, file = paste0("clean_data/pesticide/neonics_US_",resolution, ".rds"))
  
  ## Organophosphate 
  
  gen_all_arranged <- gen_toxic_all %>% 
    mutate(STATEFP = str_pad(STATE_FIPS_CODE, 2, "left", 0), 
           COUNTYFP = str_pad(COUNTY_FIPS_CODE, 2, "left", 0)) %>% 
    dplyr::select(YEAR, STATEFP, COUNTYFP, EPEST_HIGH_KG, COMPOUND) 
  
  gen_ld50 <- county_site_area %>% 
    left_join(county_area) %>% 
    arrange(STATEFP, COUNTYFP) %>% 
    left_join(gen_all_arranged) %>% 
    mutate(EPEST_HIGH_KG = ifelse(is.na(EPEST_HIGH_KG), 0, EPEST_HIGH_KG)) %>% 
    mutate(pest_area = (EPEST_HIGH_KG*area_poly)/area_county) %>% 
    group_by(site, COMPOUND, YEAR) %>% 
    summarise(pest_site = sum(pest_area)) %>% 
    left_join(dplyr::select(LD_50_clean, Active_Ingredient, Honey_Bee_Contact_LD_50_ug_bee), by = c("COMPOUND" = "Active_Ingredient")) %>% 
    mutate(pest_site_ld50 = pest_site/Honey_Bee_Contact_LD_50_ug_bee)
  
  
  saveRDS(gen_ld50, file = paste0("clean_data/pesticide/gen_toxic_US_",resolution, ".rds"))
  
  
  ### pyrethroid
  
  pyr_all_arranged <- pyrethroid_all %>% 
    mutate(STATEFP = str_pad(STATE_FIPS_CODE, 2, "left", 0), 
           COUNTYFP = str_pad(COUNTY_FIPS_CODE, 2, "left", 0)) %>% 
    dplyr::select(YEAR, STATEFP, COUNTYFP, EPEST_HIGH_KG, COMPOUND) %>% 
    mutate(COMPOUND = case_when(COMPOUND == "ALPHA CYPERMETHRIN" ~ "ALPHA-CYPERMETHRIN", 
                                COMPOUND == "CYHALOTHRIN-GAMMA" ~ "GAMMA-CYHALOTHRIN",
                                COMPOUND == "CYHALOTHRIN-LAMBDA" ~ "LAMBDA-CYHALOTHRIN",
                                TRUE ~ as.character(COMPOUND)))
  
  pyr_ld50 <- county_site_area %>% 
    left_join(county_area) %>% 
    arrange(STATEFP, COUNTYFP) %>% 
    left_join(pyr_all_arranged) %>% 
    mutate(EPEST_HIGH_KG = ifelse(is.na(EPEST_HIGH_KG), 0, EPEST_HIGH_KG)) %>% 
    mutate(pest_area = (EPEST_HIGH_KG*area_poly)/area_county) %>% 
    group_by(site, COMPOUND, YEAR) %>% 
    summarise(pest_site = sum(pest_area)) %>% 
    left_join(dplyr::select(LD_50_clean, Active_Ingredient, Honey_Bee_Contact_LD_50_ug_bee), by = c("COMPOUND" = "Active_Ingredient")) %>% 
    mutate(pest_site_ld50 = pest_site/Honey_Bee_Contact_LD_50_ug_bee)
  
  
  saveRDS(pyr_ld50, file = paste0("clean_data/pesticide/pyrethroid_US_",resolution, ".rds"))

}


### pesticide only for the US no option for country

## assign sites for 100km resolution

pest_sites(100)

## assign sites for 50km resolution

pest_sites(50)










## old function
pest_sites_old <- function(resolution){
  
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


