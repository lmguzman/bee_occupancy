library(maptools)
library(parallel)
library(raster)
library(rgdal)
library(rgeos)
library(sp)
library(spatstat)

create_sites <- function(resolution, countries){
 
  # define initial and target map projections
  prj<-"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  prj2<-"+proj=longlat"
  prj3<-"+proj=aea +lat_1=20 +lat_2=60   +lat_0=40 +lon_0=-96  +x_0=0  +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
  
  ## load political borders for countries
  countries_raster <- readOGR(dsn='raw_data/sites/Countries_WGS84/Countries_WGS84.shp') 
  
  ## re-project 
  countries_raster <- spTransform(countries_raster, CRS(prj)) #ensures the CRS is the first CRS 
  
  
  ## select countries 
  if(countries == 'US'){
    
    dd.NA <- countries_raster[c(which(countries_raster@data$CNTRY_NAME=="United States")) , ]
    
    
    ## crop islands out
    
    dd.box <- bbox2SP(n=50,
                      e=-40,
                      s=20,
                      w=-150,
                      proj4string=CRS(prj))
    dd.AM <- raster::intersect(dd.NA, dd.box)
    
  }else if(countries == 'all'){
    dd.NA <- countries_raster[c(which(countries_raster@data$CNTRY_NAME=="Canada"), 
                         which(countries_raster@data$CNTRY_NAME=="United States"), 
                         which(countries_raster@data$CNTRY_NAME=="Mexico"))
                       , ]
    
    ## crop islands out
    
    dd.box <- bbox2SP(n=87,
                      e=-40,
                      s=12,
                      w=-190,
                      proj4string=CRS(prj))
    dd.AM <- raster::intersect(dd.NA, dd.box)
  }
  
  ## Merging the countries
  
  if(countries == "all"){
    
    dd.AM@data$continent<-c("North America", "North America", "North America")
    dd.AM<-unionSpatialPolygons(dd.AM, dd.AM@data$continent)
  }

  ## Project to the one we're going to use 
  na_map <- spTransform(dd.AM, CRS(prj3)) #changes the CRS 
  
  ### create a grid with the resolution we want to use 
  
  grid <- raster(extent(na_map), resolution = c(resolution*1000,resolution*1000), crs = prj3)
  
  grid <- raster::extend(grid, c(1,1))
  
  gridPolygon <- rasterToPolygons(grid)
  
  # intercept NA map with grid
  
  intersectGridClipped <- raster::intersect(gridPolygon, na_map)
  
  ## add site number
  
  intersectGridClipped@data <- data.frame(site=paste('s', 1:length(intersectGridClipped), sep=''))
  
  area_km2 <- data.frame(site = paste('s', 1:length(intersectGridClipped), sep=''), area = sapply(intersectGridClipped@polygons, FUN = function(x) x@area)/1e+6)
  
  ## reproject again
  
  sites_clean <- spTransform(intersectGridClipped, CRS(prj2))
  
  ## save sites
  
  saveRDS(sites_clean, paste0("clean_data/sites/sites_",countries, "_", resolution,".rds"))
  saveRDS(area_km2, paste0("clean_data/sites/area_",countries, "_", resolution,".rds"))
}

## create sites for the US and US + Canada + Mexico separately
## resolutions 100 and 50
create_sites(100, "US")
create_sites(100, "all")

create_sites(50, "US")
create_sites(50, "all")


