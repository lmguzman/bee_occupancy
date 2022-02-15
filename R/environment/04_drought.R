library(ncdf4)
library(raster) 
library(rgdal)
library(ggplot2) 
library(dplyr)
library(data.table)
library(tidyr)
library(stringr)


##### U.S. Gridded Standardized Precipitation Index (SPI) from nClimGrid-Monthly ###

### best spatial resolution so far for everymonth

## download data from: https://www.drought.gov/data-maps-tools/us-gridded-standardized-precipitation-index-spi-nclimgrid-monthly

## load data
nc_data <- nc_open('/Volumes/Rasters/USC/bee_occupancy/raw_data/drought/nclimgrid-spei-gamma-01.nc')

## get basic info about lat and lon
lon_v <- ncvar_get(nc_data, "lon")
lat_v <- ncvar_get(nc_data, "lat", verbose = F)
time <- ncvar_get(nc_data, "time")

## create array with data
ndvi.array <- ncvar_get(nc_data, "spei_01") # store the data in a 3-dimensional array

## replace fill value for NAs
fillvalue <- ncatt_get(nc_data, "spei_01", "_FillValue")
fillvalue

nc_close(nc_data)

ndvi.array[ndvi.array == fillvalue$value] <- NA

## arrange data into make into data table

drought_list <- list()

time_df <- expand.grid(month = 1:12, year = 1895:2021)

counter <- 1

for(year in 1895:2021){
  
  dought_year_list <- list()
  
  for(month in 1:12){
    
    t <- which(time_df[,"month"] == month & time_df[,"year"] == year)
    
    dought_year_list[[t]] <- as.data.frame(ndvi.array[,,t]) %>%
      mutate(lon = lon_v) %>% 
      pivot_longer(names_to = 'V', values_to = 'drought', -lon) %>% 
      mutate(lat = rep(lat_v, length(lon_v))) %>% 
      dplyr::select(-V) %>% 
      filter(!is.na(drought)) %>% 
      mutate(year = time_df$year[t], month = time_df$month[t]) %>% 
      as.data.table()
    
  }
  
  drought_year <- rbindlist(dought_year_list)
  
  drought_list[[counter]] <- drought_year[,.(mean(drought, na.rm = TRUE), min(drought, na.rm = TRUE), max(drought, na.rm = TRUE), var(drought, na.rm = TRUE)), by = .(year, lat, lon)]
  
  counter <- counter + 1
  
}

drought_all <- rbindlist(drought_list)

saveRDS(drought_all, "clean_data/drought/outputs/drought_all.rds")
