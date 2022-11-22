library(ncdf4)
library(raster) 
library(rgdal)
library(ggplot2) 
library(dplyr)
library(data.table)
library(tidyr)
library(stringr)
library(sf)

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

saveRDS(drought_all, "clean_data/drought/drought_all.rds")





############# add sites to drought values ##########
## only works for US

drought_all <- readRDS("clean_data/drought/drought_all.rds")

### load sites 

sites <- readRDS(paste0("clean_data/sites/sites_counties.rds"))

## lat and long of observations

lat_lon <- unique(drought_all[, .(lat, lon)]) %>% 
  st_as_sf(
    coords = c("lon", "lat"),
    agr = "constant",
    crs = 4326,  ##WGS84   
    stringsAsFactors = FALSE,
    remove = FALSE) %>% 
  st_transform(4269)

# get county for each observation

site_obs <- st_join(lat_lon, sites, join = st_within)

lat_site <- site_obs %>% 
  st_drop_geometry() %>% 
  as.data.table()

setkeyv(lat_site, c("lat", "lon"))
setkeyv(drought_all, c("lat", "lon"))

drought_site_3 <- drought_recent[lat_site][!is.na(state_county)][, .(mean(V1), min(V2), max(V3), var(V1)), by = c("year","state_county")]

colnames(drought_site_3) <- c("year", "state_county", "mean_drought", "min_drought", "max_drought", "var_drought")

saveRDS(drought_site_3, file = paste0("clean_data/drought/drought_county.rds"))









### old 

drought_sites <- function(resolution){
  
  ### load sites 
  
  sites <- readRDS(paste0("clean_data/sites/sites_US_",resolution, ".rds"))
  
  ## lat and long of observations
  
  unique_lat_lon <- lat_lon <- distinct(drought_all[,.(lon, lat)])
  
  coordinates(lat_lon) <- ~ lon + lat
  
  proj4string(lat_lon) <- proj4string(sites)
  
  # get site number for each observation
  
  unique_lat_lon[, "site" := over(lat_lon, sites)]
  
  setkeyv(unique_lat_lon, c('lon', 'lat'))
  setkeyv(drought_all, c('lon', 'lat'))
  
  drought_site <- drought_all[unique_lat_lon]
  
  drought_site_2 <- drought_site[!is.na(site)]
  
  drought_site_3 <- drought_site_2[, .(mean(V1), min(V2), max(V3), var(V1)), by = c("year","site")]
  
  colnames(drought_site_3) <- c("year", "site", "mean_drought", "min_drought", "max_drought", "var_drought")
  
  saveRDS(drought_site_3, file = paste0("clean_data/drought/drought_US_",resolution, ".rds"))
  
}



### drought only for the US no option for country

## assign sites for 100km resolution

drought_sites(100)

## assign sites for 50km resolution

drought_sites(50)






