
## new with counties

library(dplyr)
library(data.table)
library(sf)
library(lubridate)
library(stringr)

## load data

all_obs <- fread("/Volumes/Rasters/USC/bee_occupancy/raw_data/observations/cleaned_contiguousUS_records.csv")
  
## get unique locations and years

all_small <- distinct(all_obs[,.(finalName,family, genus, eventDate, finalLatitude, finalLongitude, year, month)][!is.na(year)])

### only keep observations after 1990
# results in 2314 species

all_small_clean <- all_small[year >= 1994 & year <=2016]

## remove species with less than 10 unique lat long x years

n_obs_sp <- table(all_small_clean$finalName)

species_to_remove <- names(n_obs_sp[n_obs_sp<10])

all_small2 <- all_small_clean[!(finalName %in% species_to_remove)]

## load site data 
sites <- readRDS(paste0("clean_data/sites/sites_counties.rds"))

## add geometry for each lat and long

lat_lon <- all_small2 %>% 
  st_as_sf(
  coords = c("finalLongitude", "finalLatitude"),
  agr = "constant",
  crs = 4326,  ##WGS84   
  stringsAsFactors = FALSE,
  remove = FALSE) %>% 
  st_transform(4269)

# get county for each observation

site_obs <- st_join(lat_lon, sites, join = st_within)

# remove observations without counties

all_bees <- site_obs %>% 
  filter(!is.na(state_county))

## remove species which only have 3 years or 3 sites

bees_split <- split(all_bees, all_bees$finalName)

nyears <- sapply(bees_split, function(x) length(unique(x$year)))

nsites <- sapply(bees_split, function(x) length(unique(x$state_county)))

all_bees_geometry <- all_bees %>% 
  filter(!(finalName %in% c(names(nyears[nyears < 3]), names(nsites[nsites < 3]))))


## remove Agapostemon angelicus as it is hard to identify from A. texanus

all_bees_geometry_final <- all_bees_geometry %>% 
  filter(finalName != "Agapostemon angelicus")

# drop geometry

bee_data <- all_bees_geometry_final %>% 
  st_drop_geometry() %>% 
  as.data.table()

## count species per family

sp_fam <- bee_data[,.(finalName, family)] %>% unique()

sp_fam$family %>% table()


saveRDS(bee_data, file = paste0("clean_data/observations/observations_counties.rds"))
saveRDS(all_bees_geometry, file = paste0("clean_data/observations/observations_counties_geometry.rds"))


##### make ranges

get_ranges <- function(data, species, sites){
  
  ## get unique lat lon for the species
  
  sel_sepecies <- data %>% 
    filter(finalName == species)
  
  convex_hull <- sel_sepecies %>% 
    st_geometry() %>% 
    st_union() %>% 
    st_convex_hull() 
  
  sites_in_range <- sites[st_intersects(sites, convex_hull) %>% lengths > 0,]
  
  site_range <- sites_in_range %>% 
    st_drop_geometry() %>% unlist()
  names(site_range) <- NULL
  
  ## plotting 
  # ggplot() +
  #   geom_sf(data = sites) +
  #   geom_sf(data = sites_in_range, fill = 'blue', alpha = 0.2) +
  #   geom_sf(data = convex_hull, fill = NA, colour = 'red') +
  #   geom_sf(data = sel_sepecies, colour = 'red') +
  #   ggtitle(species)
  
  return(site_range)
}

##  make_ranges_all
  
  ## load observations
  
  all_obs <- readRDS(paste0("clean_data/observations/observations_counties_geometry.rds"))
  
  ## load sites
  
  sites <- readRDS(paste0("clean_data/sites/sites_counties.rds"))
  
  ## get all species
  
  all_sp <- unique(all_obs$finalName)
  
  ## run the make ranges for each species
  
  ranges_all_sp <- lapply(all_sp, get_ranges, data = all_obs, sites = sites)
  
  names(ranges_all_sp) <- all_sp
  
  ## remove empty ranges
  
  final_ranges <- ranges_all_sp[!sapply(ranges_all_sp, is.null)]
  
  # save
  
  saveRDS(final_ranges, file = paste0("clean_data/ranges/ranges_counties.rds"))
  




