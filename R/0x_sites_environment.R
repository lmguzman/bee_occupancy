


####### Agriculture #######

######## Step to be done with new sites!! load sites #######

## load sites
sites <- readRDS("data/clean_data/sites.rds")

## load all crop values

all_crop_values_year <- readRDS('R/agriculture/outputs/all_crop_areas_year.rds')

### figure out which categories fall on which sites

lat_lon <- all_crop_values_year[,.(x, y)]

coordinates(lat_lon) <- ~ x + y

proj4string(lat_lon) <- proj4string(sites)

# get site number for each observation

all_crop_values_year[, site := over(lat_lon, sites)]

all_crop_values_year_with_site <- all_crop_values_year[!is.na(site)]

saveRDS(all_crop_values_year_with_site, file = 'R/agriculture/outputs/crop_coordinates_3km.rds')

all_crop_values_year_with_site$crop_pres <- 1

crop_pres_year_site <- all_crop_values_year_with_site[, sum(crop_pres), by = .(year, site)]


sites$site
sites$area_sqkm <- area(sites) / 1000000

site_area <- data.frame(site = sites$site, area = sites$area_sqkm)

crop_units_area <- crop_pres_year_site %>% 
  left_join(site_area) %>% 
  mutate(crop_units_area = V1/area) %>% 
  mutate(scaled_crop_units = scale(crop_units_area, center = FALSE))

saveRDS(crop_units_area, file = 'R/agriculture/outputs/crop_units_area.rds')



###### pesticide data #####


### county map data ##

### county map https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html

# Load shapefile

us_counties <- read_sf('/Volumes/Rasters/SFU/insect_change/R/pesticide/cb_2018_us_county_20m/cb_2018_us_county_20m.shp')

## select only county and state codes

state_county_fp <- us_counties[,c("STATEFP", "COUNTYFP")]

spdf <- as_Spatial(state_county_fp)

#### load site data ###

sites <- readRDS("data/clean_data/sites.rds") 
plot(sites)

prj3<-"+proj=longlat +datum=NAD83 +no_defs"

sites_t <- spTransform(sites, CRS(prj3)) #ensures the CRS is the first CRS 

plot(sites_t)
### find sites for each county ##

sites_per_county <- over(spdf, sites_t)

site_county <- data.frame(STATE_FIPS_CODE = as.integer(state_county_fp$STATEFP), COUNTY_FIPS_CODE = as.integer(state_county_fp$COUNTYFP),
                          site = sites_per_county) %>% 
  data.table()

### join county, site and pesticide data

neonics_all <- readRDS("R/pesticide/outputs/neonics_county.rds")
gen_toxic_all <- readRDS("R/pesticide/outputs/gen_toxic_county.rds")
pyrethroid_all <- readRDS("R/pesticide/outputs/pyrethroid_county.rds")

setkeyv(site_county, c("STATE_FIPS_CODE", "COUNTY_FIPS_CODE"))
setkeyv(gen_toxic_all, c("STATE_FIPS_CODE", "COUNTY_FIPS_CODE"))
setkeyv(pyrethroid_all, c("STATE_FIPS_CODE", "COUNTY_FIPS_CODE"))
setkeyv(neonics_all, c("STATE_FIPS_CODE", "COUNTY_FIPS_CODE"))

### using State-based and other restrictions on pesticide use were not incorporated into EPest-high or EPest-low estimates. EPest-low estimates usually reflect these restrictions because they are based primarily on surveyed data. EPest-high estimates include more extensive estimates of pesticide use not reported in surveys, which sometimes include States or areas where use restrictions have been imposed. Users should consult with State and local agencies for specific use restrictions.

## using EPest-high

# perform the join, eliminating not matched rows from Right

# gen_toxic_site <- gen_toxic_all[site_county][!is.na(COMPOUND) & !is.na(site)]
# 
# gen_toxic_site_year <- gen_toxic_site[, mean(EPEST_HIGH_KG, na.rm = TRUE), by = .(YEAR, site, COMPOUND)]
# 
# saveRDS(gen_toxic_site_year, "R/pesticide/outputs/gen_toxic_site_year.rds")

gen_toxic_site_year <- readRDS("R/pesticide/outputs/gen_toxic_site_year.rds")

# pyrethroid_site <- pyrethroid_all[site_county][!is.na(COMPOUND) & !is.na(site)]
# 
# pyrethroid_site_year <- pyrethroid_site[, mean(EPEST_HIGH_KG, na.rm = TRUE), by = .(YEAR, site, COMPOUND)]
# 
# saveRDS(pyrethroid_site_year, "R/pesticide/outputs/pyrethroid_site_year.rds")

pyrethroid_site_year <- readRDS("R/pesticide/outputs/pyrethroid_site_year.rds")

# neonics_site <- neonics_all[site_county][!is.na(COMPOUND) & !is.na(site)]
# 
# neonics_site_year <- neonics_site[, mean(EPEST_HIGH_KG, na.rm = TRUE), by = .(YEAR, site, COMPOUND)]
# 
# saveRDS(neonics_site_year, "R/pesticide/outputs/neonics_site_year.rds")

neonics_site_year <- readRDS("R/pesticide/outputs/neonics_site_year.rds")



############# drought ##########

### load sites 

sites <- readRDS("data/clean_data/sites.rds")


## lat and long of observations

lat_lon <- distinct(drought_all[,.(lon, lat)])

coordinates(lat_lon) <- ~ lon + lat

proj4string(lat_lon) <- proj4string(sites)

# get site number for each observation

unique_lat_lon <- distinct(drought_all[,.(lon, lat)])

unique_lat_lon[, site := over(lat_lon, sites)]

setkeyv(unique_lat_lon, c('lon', 'lat'))
setkeyv(drought_all, c('lon', 'lat'))

drought_site <- drought_all[unique_lat_lon]

drought_site_2 <- drought_site[!is.na(site)]

drought_site_3 <- drought_site_2[, .(mean(V1), min(V2), max(V3), var(V1)), by = c("year","site")]

colnames(drought_site_3) <- c("year", "site", "mean_drought", "min_drought", "max_drought", "var_drought")

saveRDS(drought_site_3, "R/drought/outputs/drought_site.rds")










####### climate ######


#### cropping all rasters ######

## FYI, this takes a while to run on my flimsy computer

extract_values_site <- function(i){
  #load raster
  r <- raster(files[i])
  
  #extract values for site
  x1 <- exact_extract(r, sites,fun="mean")
  
  ## file name
  
  split_name <- unlist(str_split(files[i], "_"))
  
  #remove -99999.00000 and calculate mean
  #bio01_mean_values <- sapply(x1, FUN = function(x) mean(x[x !=-32768]))
  
  bio01_1_year <- data.frame(sites = sites@data, values= x1, year = split_name[4],
                             month = split_name[5], variable = split_name[3])
  
  return(bio01_1_year)
}


sites <- readRDS("data/clean_data/sites.rds")

inputDir <- "/Volumes/Rasters/SFU/insect_change/R/climate/crop_rasters"
outputDir <- "R/climate/outputs"

files <- list.files(inputDir, pattern = '.tif$', full.names = TRUE)

all_site_envirem <- mclapply(1:length(files), extract_values_site, mc.cores = 2)

saveRDS(all_site_envirem, file = "R/climate/outputs/climate.rds")

all_site_envirem <- readRDS(file = "R/climate/outputs/climate.rds")









