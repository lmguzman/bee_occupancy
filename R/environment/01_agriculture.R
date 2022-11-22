library(sf)
library(dplyr)
library(raster)
library(exactextractr)
library(data.table)

### New 

sites <- readRDS("clean_data/sites/sites_counties.RDS")

agriculture_all <- list()

count <- 1

for(year in c(2001, 2004, 2006, 2008, 2011, 2013, 2016)){
 
  agriculture_cover <- raster(paste0('/Volumes/Rasters/USC/bee_occupancy/raw_data/agriculture/nlcd_', year, '_land_cover_l48_20210604/nlcd_',year,'_land_cover_l48_20210604.img'))
  
  ## extract the fraction of the county that is agriculture i.e. == 82
  
  percent_agriculture <- exact_extract(agriculture_cover, sites, function(value, fraction) {
    sum(fraction[value == 82]) / sum(fraction)
  })
  
  agriculture_all[[count]] <- data.table(data.frame(state_county = sites$state_county, percent_agriculture = percent_agriculture, year = year))
     
  count <- count+1        
}


agriculture_all_year <-rbindlist(agriculture_all)

saveRDS(agriculture_all_year, 'clean_data/agriculture/agriculture_county.rds')






### old 


library(raster)
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
library(data.table)
library(exactextractr)  
library(cowplot)

## download crop data from: https://www.mrlc.gov/data?f%5B0%5D=category%3ALand%20Cover&f%5B1%5D=region%3Aconus&f%5B2%5D=region%3Anorth%20america
## NLCD products
## The National Land Cover Database (NLCD) provides nationwide data on land cover and land cover change at a 30m resolution with a 16-class legend based on a modified Anderson Level II classification system.

#### NLCD data comes at a 3m by 3m resolution, here we will re-project so its at 3km by 3km 
### this makes data handling easier 



## percentage of county that is agriculture/ area of county that is agriculture



### Data only for the US from 2001 to 2016

## load crop data for ##

proj <- "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"

for(year in c(2001, 2004, 2006, 2008, 2011, 2013, 2016)){
  
  ## load data
  agriculture_cover <- raster(paste0('/Volumes/Rasters/USC/bee_occupancy/raw_data/agriculture/nlcd_', year, '_land_cover_l48_20210604/nlcd_',year,'_land_cover_l48_20210604.img'))
  
  ## create dummy raster with the desired resolution
  
  resizing_raster <- raster(ext = extent(agriculture_cover), crs = proj, res = 3000)
  
  ## re-sampling the majority of points at the desired resolution
  
  crop_larger <- exact_resample(agriculture_cover, resizing_raster, "majority")
  
  ## save
  
  saveRDS(crop_larger, paste0('/Volumes/Rasters/USC/bee_occupancy/raw_data/agriculture/crop_larger/',year,'_3000m.rds'))
  
}

## NLCD products have 16 different categories, crop land is 82

##### For each 3km x 3km grid, we will get coordinates and crop values

proj <- "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"

proj2 <- '+proj=longlat'

all_crop_values <- list()

count <- 1

for(year in c(2001, 2004, 2006, 2008, 2011, 2013, 2016)){
  
  ## load re-sampled data
  
  crop_larger <- readRDS(paste0('/Volumes/Rasters/USC/bee_occupancy/raw_data/agriculture/crop_larger/',year,'_3000m.rds'))
  
  ## reassign so anything that is not a crop is NA
  
  crop_larger[crop_larger != 82] <- NA
  
  ## extract cell values 
  
  crop_vals <- raster::extract(crop_larger, 1:ncell(crop_larger))
  
  ## extract coordinates for each cell and create data table with cel values
  
  crop_table <- data.table(cbind(crop_cat = crop_vals, coordinates(crop_larger), year = year))
  
  ## remove values that are NA and different from zero
  
  crop_values_point <- crop_table[!is.na(crop_cat)  & crop_cat != 0]
  
  ## create a spatial points object with current values (which are only crop)
  
  intro_spatial <- SpatialPoints(crop_values_point[,.(x,y)], proj4string=CRS(proj))
  
  ## transform coordinates so they are in decimal latitude and longitude
  
  points_transformed <- spTransform(intro_spatial, CRS(proj2))
  
  crop_values_point[,c("x","y"):=NULL]
  
  ## create table with coordinates of where crop is for every year
  
  all_crop_values[[count]] <- data.table(cbind(crop_values_point, coordinates(points_transformed)))
  
  count <- count + 1
}


all_crop_values_year <-rbindlist(all_crop_values)

saveRDS(all_crop_values_year, 'clean_data/agriculture/agriculture.rds')



###### join agricultural data with sites #####

## load all crop values

agriculture <- readRDS('clean_data/agriculture/agriculture.rds')

agri_sites <- function(resolution){
  
  sites <- readRDS(paste0("clean_data/sites/sites_US_",resolution, ".rds"))
  
  ### figure out which categories fall on which sites
  
  lat_lon <- agriculture[,.(x, y)]
  
  coordinates(lat_lon) <- ~ x + y
  
  proj4string(lat_lon) <- proj4string(sites)
  
  # get site number for each observation
  
  agriculture[, site := over(lat_lon, sites)]
  
  agriculture_with_site <- agriculture[!is.na(site)]
  
  agriculture_with_site$crop_pres <- 1
  
  ## calculate the number of crop units per site and year
  
  crop_pres_year_site <- agriculture_with_site[, sum(crop_pres), by = .(year, site)]
  
  site_area <- readRDS(paste0("clean_data/sites/area_US_",resolution, ".rds"))
  
  ## weight number of crop units by area of sites
  
  crop_units_area <- crop_pres_year_site %>% 
    left_join(site_area) %>% 
    mutate(crop_units_area = V1/area) %>% 
    mutate(scaled_crop_units = scale(crop_units_area, center = FALSE))
  
  saveRDS(crop_units_area, file = paste0("clean_data/agriculture/agriculture_US_",resolution, ".rds"))
}

### agriculture only for the US no option for country

## assign sites for 100km resolution

agri_sites(100)

## assign sites for 50km resolution

agri_sites(50)

