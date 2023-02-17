library(sf)
library(dplyr)
library(raster)
library(exactextractr)
library(data.table)



sites <- readRDS("clean_data/sites/sites_counties.RDS")

area <- readRDS("clean_data/sites/area_counties.RDS")

agriculture_all <- list()

count <- 1

for(year in c(2008)){
  
  crop_cover <- raster(paste0('raw_data/crops/', year, '_30m_cdls/',year,'_30m_cdls.tif'))
  
  ## extract the fraction of the county that is cotton == 1
  
  #### coverage_fraction generates a raster whose values represent the fraction of each grid cell (0-1) covered by a polygon
  
  area_corn <- exact_extract(crop_cover, sites, function(value, fraction) {
    sum(fraction[value == 1]) 
  })
  
  area_soy <- exact_extract(crop_cover, sites, function(value, fraction) {
    sum(fraction[value == 5]) 
  })
  
  data.table(data.frame(state_county = sites$state_county, area_corn = area_corn, year = year, area_soy = area_soy))
  
  cotton_all[[count]] <- data.table(data.frame(state_county = sites$state_county, area_corn = area_corn, year = year))
  
  count <- count+1        
}


agriculture_all_year <-rbindlist(agriculture_all)

saveRDS(agriculture_all_year, 'clean_data/agriculture/agriculture_county.rds')


## corn
# 30-inch rows

