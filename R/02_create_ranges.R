
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

## clean month

# Extract date from event date
all_small$date <- ymd_hms(all_small$eventDate) 

# extrcat date from month
all_small[, date2 := fifelse(str_detect(month, "\\d{4}\\-\\d{2}\\-\\d{2} to"), ymd(str_extract(month, "\\d{4}\\-\\d{2}\\-\\d{2}")), ymd(date))]

# extract date from event date by first replacing / by -
all_small[, date3 := fifelse(is.na(date) & is.na(date2) & eventDate != "", mdy(str_replace(eventDate, "/", "-")), mdy(NA))]

# extract date from the month column that is written as a date
all_small[, date4 := fifelse(is.na(date) & is.na(date2) & is.na(date3) & str_detect(month, "\\d{4}\\-\\d{2}\\-\\d{2}") , ymd(month), ymd(NA))]

# from all of the dates 1-4 above
all_small$month_clean <- case_when(!is.na(all_small$date) ~ month(all_small$date), 
                                   !is.na(all_small$date2) ~ month(all_small$date2), 
                                   !is.na(all_small$date3) ~ month(all_small$date3), 
                                   !is.na(all_small$date4) ~ month(all_small$date4), 
                                   # for the remainder, do as numeric of month but skip the months that are 0
                                   is.na(all_small$date) & is.na(all_small$date2) & 
                                     is.na(all_small$date3) & is.na(all_small$date4) & all_small$month != "0" ~ as.numeric(all_small$month))

# remove all observations where month is 0 and we couldn't extract month from another way
all_small <- all_small[!(is.na(all_small$date) & is.na(all_small$date2) & 
                           is.na(all_small$date3) & is.na(all_small$date4) & all_small$month == "0")]

# clean the months that are abbreviated
month_to_clean <- all_small[is.na(month_clean)]$month

month_text <- str_extract_all(month_to_clean, "[a-zA-Z]*")

month_text_list <- lapply(month_text, FUN = function(x) x[x!=""])

month_text_clean <-unlist(lapply(month_text_list, FUN = function(x) ifelse(length(x) == 0, NA, x)))

all_small[is.na(month_clean)]$month_clean <- sapply(month_text_clean, FUN = function(x) ifelse(length(str_which(x, month.abb)) == 0,
                                                                                               NA, str_which(x, month.abb)))

# remove months -1, -2, 18 and 20

all_small_clean <- all_small[!is.na(month_clean) & month != -1 & month != -2 & month != 18 & month != 20, 
                             .(finalName, family, genus, finalLatitude, finalLongitude, year, month_clean)]  


## remove species with less than 5 unique lat long x years

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

## remove species which only have two years or two sites

bees_split <- split(all_bees, all_bees$finalName)

nyears <- sapply(bees_split, function(x) length(unique(x$year)))

nsites <- sapply(bees_split, function(x) length(unique(x$state_county)))

all_bees_geometry <- all_bees %>% 
  filter(!(finalName %in% c(names(nyears[nyears < 3]), names(nsites[nsites < 10]))))

bee_data <- all_bees_geometry %>% 
  st_drop_geometry() %>% 
  as.data.table()

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
  












##### OLD #########

library(dplyr)
library(data.table)
library(sp)
library(rgeos)
library(lubridate)
library(stringr)

## further subset data to plausible species

bee_ready <- function(resolution, countries){
  
  ## load data
  
  if(countries == 'US'){
    
    all_obs <- fread("/Volumes/Rasters/USC/bee_occupancy/raw_data/observations/cleaned_contiguousUS_records.csv")
    
  }else if(countries == 'all'){
    
    all_obs <- fread("/Volumes/Rasters/USC/bee_occupancy/raw_data/observations/NorAmer_highQual_only_ALLfamilies.csv")
    
  }

  ## get unique locations and years
  
  all_small <- distinct(all_obs[,.(finalName,family, genus, eventDate, finalLatitude, finalLongitude, year, month)][!is.na(year)])
  
  ## clean month
  
  # Extrac date from event date
  all_small$date <- ymd_hms(all_small$eventDate) 

  # extrat date from month
  all_small[, date2 := fifelse(str_detect(month, "\\d{4}\\-\\d{2}\\-\\d{2} to"), ymd(str_extract(month, "\\d{4}\\-\\d{2}\\-\\d{2}")), ymd(date))]
  
  # extract date from event date by first replacing / by -
  all_small[, date3 := fifelse(is.na(date) & is.na(date2) & eventDate != "", mdy(str_replace(eventDate, "/", "-")), mdy(NA))]

  # extract date from the month column that is written as a date
  all_small[, date4 := fifelse(is.na(date) & is.na(date2) & is.na(date3) & str_detect(month, "\\d{4}\\-\\d{2}\\-\\d{2}") , ymd(month), ymd(NA))]
  
  # from all of the dates 1-4 above
  all_small$month_clean <- case_when(!is.na(all_small$date) ~ month(all_small$date), 
                                     !is.na(all_small$date2) ~ month(all_small$date2), 
                                     !is.na(all_small$date3) ~ month(all_small$date3), 
                                     !is.na(all_small$date4) ~ month(all_small$date4), 
                                     # for the remainder, do as numeric of month but skip the months that are 0
                                     is.na(all_small$date) & is.na(all_small$date2) & 
                                       is.na(all_small$date3) & is.na(all_small$date4) & all_small$month != "0" ~ as.numeric(all_small$month))
  
  # remove all observations where month is 0 and we couldn't extract month from another way
  all_small <- all_small[!(is.na(all_small$date) & is.na(all_small$date2) & 
              is.na(all_small$date3) & is.na(all_small$date4) & all_small$month == "0")]
  
  # clean the months that are abbreviated
  month_to_clean <- all_small[is.na(month_clean)]$month
  
  month_text <- str_extract_all(month_to_clean, "[a-zA-Z]*")
  
  month_text_list <- lapply(month_text, FUN = function(x) x[x!=""])
  
  month_text_clean <-unlist(lapply(month_text_list, FUN = function(x) ifelse(length(x) == 0, NA, x)))
  
  all_small[is.na(month_clean)]$month_clean <- sapply(month_text_clean, FUN = function(x) ifelse(length(str_which(x, month.abb)) == 0,
                                                                                           NA, str_which(x, month.abb)))
  
  # remove months -1, -2, 18 and 20
  
  all_small_clean <- all_small[!is.na(month_clean) & month != -1 & month != -2 & month != 18 & month != 20, 
                               .(finalName, family, genus, finalLatitude, finalLongitude, year, month_clean)]  
  

  ## remove species with less than 5 unique lat long x years
  
  n_obs_sp <- table(all_small_clean$finalName)
  
  species_to_remove <- names(n_obs_sp[n_obs_sp<10])
  
  all_small2 <- all_small_clean[!(finalName %in% species_to_remove)]
  
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
  
  bee_data <- all_bees[!(finalName %in% c(names(nyears[nyears < 3]), names(nsites[nsites < 10])))]
  
  saveRDS(bee_data, file = paste0("clean_data/observations/observations_",countries,"_",resolution, ".rds"))
}

### get datasets ready for US and all for 100 and 50km 

bee_ready(100, 'US')

#bee_ready(50, 'US')

#bee_ready(100, 'all')

#bee_ready(50, 'all')



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

# make_ranges_all(50, 'US')
# 
# make_ranges_all(100, 'all')
# 
# make_ranges_all(50, 'all')





