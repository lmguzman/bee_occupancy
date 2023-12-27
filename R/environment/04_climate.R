### script to extract climate data from CHELSA 

library(stringr)
library(dplyr)
library(parallel)
library(tidyr)
library(exactextractr)
library(sf)
library(raster)

##### download data from chelsa time series https://chelsa-climate.org/timeseries/

####### extract values from counties  ######

extract_values_site <- function(i){

  #load raster
  r <- raster(files[i])
  
  #extract values for site
  x1 <- exact_extract(r, sites,fun="mean")
  
  ## file name
  
  split_name <- unlist(str_split(files[i], "_"))
  
  #remove -99999.00000 and calculate mean
  #bio01_mean_values <- sapply(x1, FUN = function(x) mean(x[x !=-32768]))
  
  bio01_1_year <- data.frame(state_county = sites$state_county, values= x1, year = split_name[5],
                             month = split_name[6], variable = split_name[4])
  
  return(bio01_1_year)
}


## load sites 
sites <- readRDS("clean_data/sites/sites_counties.RDS")

## run the extraction

inputDir <- "/Volumes/Rasters/USC/bee_occupancy/raw_data/climate/rasters"

files <- list.files(inputDir, pattern = '.tif$', full.names = TRUE)


#### for loop ###

all_site_envirem <- list()

for(i in 687:length(files)){
  
  print(i)
  
  all_site_envirem[[i]] <- extract_values_site(i)
  
}

## save
saveRDS(all_site_envirem, file = paste0("clean_data/climate/climate_counties.rds"))

