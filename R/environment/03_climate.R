library(rgeos)
library(envirem)
library(maps)
library(viridis)
library(stringr)
library(dplyr)
library(ggplot2)
library(parallel)
library(purrr)
library(tidyr)
library(exactextractr)  

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
  r <- raster(files[i])
  
  #extract values for site
  x1 <- exact_extract(r, sites,fun="mean")
  
  ## file name
  
  split_name <- unlist(str_split(files[i], "_"))
  
  #remove -99999.00000 and calculate mean
  #bio01_mean_values <- sapply(x1, FUN = function(x) mean(x[x !=-32768]))
  
  bio01_1_year <- data.frame(sites = sites@data, values= x1, year = split_name[5],
                             month = split_name[6], variable = split_name[4])
  
  return(bio01_1_year)
}

extract_values_all_rasters <- function(resolution, countries){
  
  ## load sites 
  sites <- readRDS(paste0("clean_data/sites/sites_", countries,"_",resolution, ".rds"))
  
  ## run the extraction
  inputDir <- "/Volumes/Rasters/SFU/insect_change/R/climate/crop_rasters"
  
  #inputDir <- "/Volumes/Rasters/USC/bee_occupancy/raw_data/climate/crop_rasters"
  
  files <- list.files(inputDir, pattern = '.tif$', full.names = TRUE)
  
  all_site_envirem <- mclapply(1:length(files), extract_values_site, mc.cores = 2)
  
  ## save
  saveRDS(all_site_envirem, file = paste0("clean_data/climate/climate_",countries,"_",resolution, ".rds"))
  
}


### extract climate for all resolutions and coutries

extract_values_all_rasters(100, 'US')

extract_values_all_rasters(50, 'US')


extract_values_all_rasters(100, 'all')

extract_values_all_rasters(50, 'all')



