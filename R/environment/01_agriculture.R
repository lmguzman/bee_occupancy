### script to extract the agriculture data from the NLCD data set 

library(sf)
library(dplyr)
library(raster)
library(exactextractr)
library(data.table)

### load sites  

sites <- readRDS("clean_data/sites/sites_counties.RDS")

agriculture_all <- list()

count <- 1

## loop for every year of data 

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



