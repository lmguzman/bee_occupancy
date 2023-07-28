### script to extract climate data from CHELSA 

library(stringr)
library(dplyr)
library(parallel)
library(tidyr)
library(exactextractr)
library(sf)
library(raster)

##### download data from chelsa cruts https://chelsa-climate.org/chelsacruts/library(raster)

inputDir <- "/Volumes/Rasters/USC/bee_occupancy/raw_data/climate/rasters"

files_all <- list.files(inputDir, pattern = '.tif$', full.names=TRUE)

### crop rasters to only north america to make them easier to work with

dd.box <- bbox2SP(n=87,
                  e=-40,
                  s=12,
                  w=-190)


for(f in 1:length(files_all)){
  
  ## load raster
  climate_raster <- raster(files_all[f])
  
  ## intersect with box
  climate_raster2 <- raster::intersect(climate_raster, dd.box)
  
  ## write new raster
  writeRaster(climate_raster2, paste0("/Volumes/Rasters/USC/bee_occupancy/raw_data/climate/crop_rasters/", str_remove(files[f], "/Volumes/Rasters/USC/bee_occupancy/raw_data/climate/rasters/")))
}


####### assign sites  ######

extract_values_site <- function(i){

  #load raster
  r <- raster(files_to_do[i])
  
  #extract values for site
  x1 <- exact_extract(r, sites,fun="mean")
  
  ## file name
  
  split_name <- unlist(str_split(files_to_do[i], "_"))
  
  #remove -99999.00000 and calculate mean
  #bio01_mean_values <- sapply(x1, FUN = function(x) mean(x[x !=-32768]))
  
  bio01_1_year <- data.frame(state_county = sites$state_county, values= x1, year = split_name[6],
                             month = split_name[7], variable = split_name[5])
  
  return(bio01_1_year)
}


## load sites 
sites <- readRDS("clean_data/sites/sites_counties.RDS")

## run the extraction

inputDir <- "/Volumes/Rasters/USC/bee_occupancy/raw_data/climate/crop_rasters"

files <- list.files(inputDir, pattern = '.tif$', full.names = TRUE)

files_to_do <- files[str_extract(files, "\\d\\d\\d\\d") %in% as.character(1990:2016)]

all_site_envirem <- mclapply(1:length(files_to_do), extract_values_site, mc.cores = 2)

## save
saveRDS(all_site_envirem, file = paste0("clean_data/climate/climate_counties.rds"))



