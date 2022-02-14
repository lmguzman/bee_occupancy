


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


