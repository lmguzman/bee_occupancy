library(dplyr)
library(data.table)
library(sp)
library(rgeos)

## further subset data to plausible species

bee_ready <- function(resolution, countries){
  
  ## load data
  
  if(countries == 'US'){
    
    all_obs <- fread("/Volumes/Rasters/USC/bee_occupancy/raw_data/observations/cleaned_contiguousUS_records.csv")
    
  }else if(countries == 'all'){
    
    all_obs <- fread("/Volumes/Rasters/USC/bee_occupancy/raw_data/observations/NorAmer_highQual_only_ALLfamilies.csv")
    
  }

  ## get unique locations and years
  
  all_small <- distinct(all_obs[,.(finalName,family, finalLatitude, finalLongitude, year)][!is.na(year)])
  
  ## remove species with less than 5 unique lat long x years
  
  n_obs_sp <- table(all_small$finalName)
  
  species_to_remove <- names(n_obs_sp[n_obs_sp<5])
  
  all_small2 <- all_small[!(finalName %in% species_to_remove)]
  
  ## load site data 
  sites <- readRDS(paste0("clean_data/sites/sites_",countries,"_",resolution, ".rds"))
  
  ## lat and long of observations
  
  lat_lon <- all_small2[,.(finalLongitude, finalLatitude)]
  
  coordinates(lat_lon) <- ~ finalLongitude + finalLatitude
  
  proj4string(lat_lon) <- proj4string(sites)
  
  # get site number for each observation
  
  all_small2[, site := over(lat_lon, sites)]
  
  all_bees <- all_small2[!is.na(site)]
  
  ## remove species which only have two years or two sites
  
  bees_split <- split(all_bees, all_bees$finalName)
  
  nyears <- sapply(bees_split, function(x) length(unique(x[,year])))
  
  nsites <- sapply(bees_split, function(x) length(unique(x[,site])))
  
  bee_data <- all_bees[!(finalName %in% c(names(nyears[nyears < 2]), names(nsites[nsites < 2])))]
  
  saveRDS(bee_data, file = paste0("clean_data/observations/observations_",countries,"_",resolution, ".rds"))
}

### get datasets ready for US and all for 100 and 50km 

bee_ready(100, 'US')

bee_ready(50, 'US')

bee_ready(100, 'all')

bee_ready(50, 'all')



##### make ranges

get_ranges <- function(data, species, sites){
  
  ## get unique lat lon for the species
  
  lat_lon <- unique(data[finalName == species,.(finalLongitude, finalLatitude)])
  
  coordinates(lat_lon) <- ~ finalLongitude + finalLatitude
  
  proj4string(lat_lon) <- proj4string(sites)
  
  ch <- gConvexHull(lat_lon)
  
  sites_in_range <- !is.na(over(sites, ch))
  
  site_range <- sites[sites_in_range,]
  
  return(site_range)
}

make_ranges_all <- function(resolution, countries){

  ## load observations

    all_obs <- readRDS(paste0("clean_data/observations/observations_",countries,"_",resolution, ".rds"))
    
    ## load sites
    
    sites <- readRDS(paste0("clean_data/sites/sites_",countries,"_",resolution, ".rds"))
    
    ## get all species
    
    all_sp <- unique(all_obs$finalName)
  
    ## run the make ranges for each species
    
    ranges_all_sp <- lapply(all_sp, get_ranges, data = all_obs, sites = sites)
    
    names(ranges_all_sp) <- all_sp
    
    ## remove empty ranges
    
    final_ranges <- ranges_all_sp[!sapply(ranges_all_sp, is.null)]
    
    # save
    
    saveRDS(final_ranges, file = paste0("clean_data/ranges/ranges_",countries,"_",resolution, ".rds"))
    
}


### make ranges for US and all for 100 and 50km 

make_ranges_all(100, 'US')

make_ranges_all(50, 'US')

make_ranges_all(100, 'all')

make_ranges_all(50, 'all')





