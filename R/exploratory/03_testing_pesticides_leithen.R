library(dplyr)
library(data.table)
library(ggplot2)
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
library(sf)

########### previous checking with Leithen ##########

agriculture <- readRDS('clean_data/agriculture/agriculture.rds')

resolution <- 100

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


#### plot ##

site_df <- fortify(sites)


sites_ag_n <- mutate(site_df, site = paste0("s", id)) %>% 
  left_join(crop_units_area) %>% 
  mutate(V1 = ifelse(is.na(V1), 0, V1))


p0 <- ggplot(data = sites_ag_n, aes(x = long, y = lat, group = group, fill = V1)) +
  geom_polygon() +
  geom_path(color = "white", size = 0.2)  + 
  theme_cowplot() +
  facet_wrap(~year) +
  theme(panel.background= element_rect(color="black")) +
  theme(axis.title = element_blank(), axis.text = element_blank()) +
  scale_fill_distiller(palette = 'BuPu')

p0


## pesticides 

family_load <- "Apidae"
my.data <- readRDS(paste0("clean_data/data_prepared/my_data_env_genus_US_100_1997_2016_",family_load,".rds"))


my.data[[1]]$pesticide1

my.data[[1]]$pesticide2

cor(my.data[[1]]$pesticide1, my.data[[1]]$pesticide2)

pest1_df <- data.frame(my.data[[1]]$pesticide1) %>% 
  tibble::rownames_to_column("site") %>% 
  pivot_longer(names_to = "year", values_to = "pest1", -site)

pest2_df <- data.frame(my.data[[1]]$pesticide2) %>% 
  tibble::rownames_to_column("site") %>% 
  pivot_longer(names_to = "year", values_to = "pest2", -site)

pest3_df <- data.frame(my.data[[1]]$pesticide3) %>% 
  tibble::rownames_to_column("site") %>% 
  pivot_longer(names_to = "year", values_to = "pest3", -site)


agriculture_df <- data.frame(my.data[[1]]$agriculture) %>% 
  tibble::rownames_to_column("site") %>% 
  pivot_longer(names_to = "year", values_to = "agriculture", -site)

pest1_df %>% 
  left_join(pest3_df) %>% 
  ggplot(aes(x = pest1, y = pest3, colour = year)) + geom_point()

pest2_df %>% 
  left_join(pest3_df) %>% 
  ggplot(aes(x = pest2, y = pest3, colour = year)) + geom_point()


pest1_df %>% 
  left_join(pest2_df) %>% 
  ggplot(aes(x = pest1, y = pest2, colour = year)) + geom_point()

pest1_df %>% 
  left_join(agriculture_df) %>% 
  filter(year == "yr2013") %>% 
  ggplot(aes(x = agriculture, y = pest1, colour = year)) + geom_point()

pest2_df %>% 
  left_join(agriculture_df) %>% 
  ggplot(aes(x = agriculture, y = pest2, colour = year)) + geom_point()


pest3_df %>% 
  left_join(agriculture_df) %>% 
  ggplot(aes(x = agriculture, y = pest3)) + geom_point() +
  facet_wrap(~year)


pest1yr <- pest1_df %>% 
  filter(year =="yr2013")

site_pest1 <- site_df %>% 
  mutate(site = paste0("s", str_pad(id, 3, "0", side = "left"))) %>% 
  left_join(pest1yr) %>% 
  filter(!is.na(pest1)) %>%
  filter(pest1 != 0)

ggplot(data = site_pest1, aes(x = long, y = lat, group = group, fill = pest1)) +
  geom_polygon() +
  geom_path(color = "white", size = 0.2)  + 
  theme_cowplot() +
  facet_wrap(~year) +
  theme(panel.background= element_rect(color="black")) +
  theme(axis.title = element_blank(), axis.text = element_blank()) +
  scale_fill_distiller(palette = 'BuPu')



##### double check agriculture and pesticides ####

#check the agriculture at the county level

## agriculture
agriculture <- readRDS('clean_data/agriculture/agriculture.rds')

agriculture %>% 
  ggplot(aes(x = x, y = y)) + geom_point()


## neonics

neonics_all <- readRDS("clean_data/pesticide/neonics_county.rds")

# Load shapefile

us_counties <- read_sf('/Volumes/Rasters/USC/bee_occupancy/raw_data/pesticide/cb_2018_us_county_20m/cb_2018_us_county_20m.shp')

## select only county and state codes

state_county_fp <- us_counties[,c("STATEFP", "COUNTYFP")]

spdf <- as_Spatial(state_county_fp)


prj3<-"+proj=longlat +datum=NAD83 +no_defs"

sites_t <- spTransform(sites, CRS(prj3))

## find the counties per site

sites_per_county <- over(spdf, sites_t)

### area of itercection of each county with that site --
### calculate the pesticide -- area of intercetion of the county divided by area of county multiplied by pesticide

sites_per_county <- intersect(spdf, sites_t)

intersect(spdf, sites_t[1])

county_site_interception <- st_intersection(st_as_sf(spdf), st_as_sf(sites_t)[1])

## already gives me the chopped up counties
## calculate the area for each row

area_polygon <- st_area(county_site_interception)

# area of county that intersects site
county_site_area <- data.frame(STATEFP = county_site_interception$STATEFP, COUNTYFP = county_site_interception$COUNTYFP, site = county_site_interception$site, area_poly = as.numeric(area_polygon))

## total area of county
area_county <- st_area(state_county_fp)

county_area <- data.frame(STATEFP = state_county_fp$STATEFP, COUNTYFP = state_county_fp$COUNTYFP, area_county = as.numeric(area_county))


## match county site area to county area

imidacloprid_1 <- imidacloprid_2014[COMPOUND == 'IMIDACLOPRID'] %>% 
  mutate(STATEFP = str_pad(STATE_FIPS_CODE, 2, "left", 0), 
         COUNTYFP = str_pad(COUNTY_FIPS_CODE, 2, "left", 0)) %>% 
  dplyr::select(STATEFP, COUNTYFP, EPEST_HIGH_KG)


neonic_1 <- neonics_all[YEAR == 2014] %>% 
  mutate(STATEFP = str_pad(STATE_FIPS_CODE, 2, "left", 0), 
         COUNTYFP = str_pad(COUNTY_FIPS_CODE, 2, "left", 0)) %>% 
  dplyr::select(STATEFP, COUNTYFP, EPEST_HIGH_KG, COMPOUND) 

### per pesticide 

county_site_area %>% 
  left_join(county_area) %>% 
  arrange(STATEFP, COUNTYFP) %>% 
  left_join(imidacloprid_1) %>% 
  mutate(EPEST_HIGH_KG = ifelse(is.na(EPEST_HIGH_KG), 0, EPEST_HIGH_KG)) %>% 
  mutate(pest_area = (EPEST_HIGH_KG*area_poly)/area_county) %>% 
  group_by(site) %>% 
  summarise(pest_site = sum(pest_area))



summed_pest_county_ld50 <- county_site_area %>% 
  left_join(county_area) %>% 
  arrange(STATEFP, COUNTYFP) %>% 
  left_join(neonic_1) %>% 
  mutate(EPEST_HIGH_KG = ifelse(is.na(EPEST_HIGH_KG), 0, EPEST_HIGH_KG)) %>% 
  mutate(pest_area = (EPEST_HIGH_KG*area_poly)/area_county) %>% 
  group_by(site, COMPOUND) %>% 
  summarise(pest_site = sum(pest_area)) %>% 
  left_join(dplyr::select(LD_50_clean, Active_Ingredient, Honey_Bee_Contact_LD_50_ug_bee), by = c("COMPOUND" = "Active_Ingredient")) %>% 
  mutate(pest_site_ld50 = pest_site/Honey_Bee_Contact_LD_50_ug_bee)

summed_pest_county_ld50 %>% 
  ggplot(aes(x = pest_site_ld50)) + geom_histogram() +
  facet_wrap(~COMPOUND) +
  scale_x_log10()


summed_pest_county_ld50 %>% 
  group_by(site) %>% 
  summarise(sum_pests = sum(pest_site_ld50, na.rm = TRUE)) %>% 
  ggplot(aes(x = sum_pests)) + geom_histogram() +
  scale_x_log10()

summed_pest_county_ld50 %>% 
  mutate(pest_site_ld50_log10 = log10(pest_site_ld50)) %>% 
  group_by(site) %>% 
  summarise(sum_pests = sum(pest_site_ld50_log10, na.rm = TRUE)) %>% 
  ggplot(aes(x = sum_pests)) + geom_histogram()

## divide by LD50

### harvest LD50s

library(rvest)

LD_50_mn<- read_html("https://www.mda.state.mn.us/protecting/bmps/pollinators/beetoxicity") %>% 
  html_table(fill = TRUE)

LD_50_mn <- LD_50_mn[[1]]

# fix colnames
colnames(LD_50_mn) <- str_replace(str_remove_all(str_replace_all(colnames(LD_50_mn), " ", "_"), "\\(|\\)"), "\\/", "_")

#remove commas and stars, make numeric
LD_50_clean <- LD_50_mn %>% 
  mutate(Honey_Bee_Contact_LD_50_ug_bee = as.numeric(str_remove_all(str_replace_all(Honey_Bee_Contact_LD_50_ug_bee, ",", ""), "\\*")),
         Honey_Bee_Contact_LD_50_ppb = as.numeric(str_remove_all(str_replace_all(Honey_Bee_Contact_LD_50_ppb, ",", ""), "\\*")),
         Honey_Bee_Oral_LD_50_ug_bee = as.numeric(str_remove_all(str_replace_all(Honey_Bee_Oral_LD_50_ug_bee, ",", ""), "\\*")),
         Honey_Bee_Oral_LD_50_ppb = as.numeric(str_remove_all(str_replace_all(Honey_Bee_Oral_LD_50_ppb, ",", ""), "\\*")))

LD_50_clean
LD_50_clean %>% View()
summary(LD_50_clean)

LD_50_clean %>% 
  filter(Active_Ingredient %in% unique(neonics_all$COMPOUND))

unique(neonics_all$COMPOUND) %in% LD_50_clean$Active_Ingredient

LD_50_clean %>% 
  ggplot(aes(x = Honey_Bee_Contact_LD_50_ug_bee, y = Honey_Bee_Oral_LD_50_ug_bee)) + geom_point() +
  scale_x_log10() + scale_y_log10()



site_county <- data.frame(STATE_FIPS_CODE = as.integer(state_county_fp$STATEFP), COUNTY_FIPS_CODE = as.integer(state_county_fp$COUNTYFP),
                          site = sites_per_county) %>% 
  data.table()

## set joining keys
setkeyv(site_county, c("STATE_FIPS_CODE", "COUNTY_FIPS_CODE"))
setkeyv(neonics_all, c("STATE_FIPS_CODE", "COUNTY_FIPS_CODE"))


# perform the join, eliminating not matched rows from Right

names_df <- c("year", "site", "compound", "epest_high", "epest_low")

## organophosphates

neonics_site <- neonics_all[site_county][!is.na(COMPOUND)]

#neonics_site_year <- neonics_site[, .(mean(EPEST_HIGH_KG, na.rm = TRUE), mean(EPEST_LOW_KG, na.rm = TRUE)), by = .(YEAR, site, COMPOUND)]

#colnames(neonics_site_year) <- names_df

imidacloprid_2014 <- neonics_site[YEAR == 2014 & COMPOUND == 'IMIDACLOPRID']

agriculture_2013 <- agriculture[year == 2013]



## join agriculture 2013 with county 

lat_lon <- agriculture_2013[,.(x, y)]

coordinates(lat_lon) <- ~ x + y

proj4string(lat_lon) <- proj4string(spdf)

agriculture_2013 <- cbind(agriculture_2013, over(lat_lon, spdf))


agriculture_county <- agriculture_2013[!is.na(STATEFP)][, .N, by = .(STATEFP, COUNTYFP)]

agriculture_county2 <- agriculture_county %>% 
  mutate(STATE_FIPS_CODE = as.integer(STATEFP), 
         COUNTY_FIPS_CODE = as.integer(COUNTYFP))

setkeyv(agriculture_county2, c("STATE_FIPS_CODE", "COUNTY_FIPS_CODE"))
setkeyv(imidacloprid_2014, c("STATE_FIPS_CODE", "COUNTY_FIPS_CODE"))

all_counties <- data.frame(STATE_FIPS_CODE = as.integer(state_county_fp$STATEFP), COUNTY_FIPS_CODE = as.integer(state_county_fp$COUNTYFP)) %>% 
  data.table()

imidacloprid_agrilculture <- imidacloprid_2014[agriculture_county2]


imidacloprid_agriculture_full <- imidacloprid_agrilculture %>% 
  dplyr::select(-site, -STATEFP, -COUNTYFP, -YEAR) %>% 
  right_join(all_counties) %>% 
  mutate(EPEST_LOW_KG = ifelse(is.na(EPEST_LOW_KG), 0, EPEST_LOW_KG),
         EPEST_HIGH_KG = ifelse(is.na(EPEST_HIGH_KG), 0, EPEST_HIGH_KG), 
         N = ifelse(is.na(N), 0, N))

imidacloprid_agriculture_full %>% 
  ggplot(aes(x = N, y = EPEST_LOW_KG)) + geom_point() +
  xlab("Number of 3km agriculture plots in county") +
  ylab("Estimated imidacloprid use - Low")

imidacloprid_agriculture_full %>% 
  ggplot(aes(x = N, y = EPEST_HIGH_KG)) + geom_point() +
  xlab("Number of 3km agriculture plots in county") +
  ylab("Estimated imidacloprid use - High")


#Check the pesticide in nevada, west
#why it looks like there is no relationship between pesticide aplication and agriculture

state_code <- 32

imidacloprid_agriculture_full %>% 
  filter(STATE_FIPS_CODE == st) %>% 
  ggplot(aes(x = N, y = EPEST_HIGH_KG)) + geom_point() +
  xlab("Number of 3km agriculture plots in county") +
  ylab("Estimated imidacloprid use - High")

state_codes <- read.csv("raw_data/counties_states/STATE_FIPS_CODE.txt")

state_codes <- state_codes %>% 
  filter(!(State %in% c("Alaska", "Hawaii"))) %>% 
  mutate(STATE_FIPS_CODE = str_pad(STATE_FIPS_CODE, 2, side = 'left', 0))

state_county_fp <- us_counties[,c("STATEFP", "COUNTYFP")]

spdf <- as_Spatial(state_county_fp)

for(st in state_codes$STATE_FIPS_CODE){
  
  nevada <- state_county_fp[state_county_fp$STATEFP == st,]

  county_df <- data.frame(COUNTY_FIPS_CODE = as.numeric(nevada$COUNTYFP), id = as.character(1:length(nevada$COUNTYFP)))
  
  nevada_df <- fortify(as_Spatial(nevada))  %>% 
    left_join(county_df) %>% 
    left_join(filter(imidacloprid_agriculture_full, STATE_FIPS_CODE == as.numeric(st)))
  
  agri_plot <- ggplot(data = nevada_df, aes(x = long, y = lat, group = group, fill = N)) +
    geom_polygon() +
    geom_path(color = "white", size = 0.2)  + 
    theme_cowplot() +
    theme(panel.background= element_rect(color="black")) +
    theme(axis.title = element_blank(), axis.text = element_blank()) +
    scale_fill_distiller(palette = 'BuPu', name = 'N ag plots') +
    ggtitle(state_codes[state_codes$STATE_FIPS_CODE == st,1])
  
  pest_plot <- ggplot(data = nevada_df, aes(x = long, y = lat, group = group, fill = EPEST_HIGH_KG)) +
    geom_polygon() +
    geom_path(color = "white", size = 0.2)  + 
    theme_cowplot() +
    theme(panel.background= element_rect(color="black")) +
    theme(axis.title = element_blank(), axis.text = element_blank()) +
    scale_fill_distiller(palette = 'BuPu', name = 'imid high')+
    ggtitle("")
  
  
  agri_pest_plot <- plot_grid(agri_plot, pest_plot)
  
  ggsave(agri_pest_plot, filename = paste0('other/imi_state_ag/', state_codes[state_codes$STATE_FIPS_CODE == st,1], '.jpeg'))
  
}

############# now check the actual pesticide calcualtions that we do per county ############

neonics_2014 <- neonics_site[YEAR == 2014]

setkeyv(neonics_2014, c("STATE_FIPS_CODE", "COUNTY_FIPS_CODE"))

all_counties <- data.frame(STATE_FIPS_CODE = as.integer(state_county_fp$STATEFP), COUNTY_FIPS_CODE = as.integer(state_county_fp$COUNTYFP)) %>% 
  data.table()

neonic_agrilculture <- neonics_2014[agriculture_county2]

neonic_agriculture_full <- neonic_agrilculture %>% 
  right_join(all_counties) %>% 
  mutate(EPEST_LOW_KG = ifelse(is.na(EPEST_LOW_KG), 0, EPEST_LOW_KG),
         EPEST_HIGH_KG = ifelse(is.na(EPEST_HIGH_KG), 0, EPEST_HIGH_KG), 
         N = ifelse(is.na(N), 0, N))

pest_agriculture_full <- neonic_agriculture_full %>% 
  mutate(logV1 = log(EPEST_HIGH_KG+1)) %>% 
  dplyr::select(-site, -STATEFP, -COUNTYFP, -YEAR, -EPEST_LOW_KG, -EPEST_HIGH_KG) %>% 
  group_by(STATE_FIPS_CODE, COUNTY_FIPS_CODE, N) %>% 
  summarise(pest_neonic = sum(logV1))
  

pest_agriculture_full %>% 
  ggplot(aes(x = N, y = pest_neonic)) + geom_point() +
  xlab("Number of 3km agriculture plots in county") +
  ylab("Sum of log neonics")



for(st in state_codes$STATE_FIPS_CODE){
  
  nevada <- state_county_fp[state_county_fp$STATEFP == st,]
  
  county_df <- data.frame(COUNTY_FIPS_CODE = as.numeric(nevada$COUNTYFP), id = as.character(1:length(nevada$COUNTYFP)))
  
  nevada_df <- fortify(as_Spatial(nevada))  %>% 
    left_join(county_df) %>% 
    left_join(filter(pest_agriculture_full, STATE_FIPS_CODE == as.numeric(st)))
  
  agri_plot <- ggplot(data = nevada_df, aes(x = long, y = lat, group = group, fill = N)) +
    geom_polygon() +
    geom_path(color = "white", size = 0.2)  + 
    theme_cowplot() +
    theme(panel.background= element_rect(color="black")) +
    theme(axis.title = element_blank(), axis.text = element_blank()) +
    scale_fill_distiller(palette = 'BuPu', name = 'N ag plots') +
    ggtitle(state_codes[state_codes$STATE_FIPS_CODE == st,1])
  
  pest_plot <- ggplot(data = nevada_df, aes(x = long, y = lat, group = group, fill = pest_neonic)) +
    geom_polygon() +
    geom_path(color = "white", size = 0.2)  + 
    theme_cowplot() +
    theme(panel.background= element_rect(color="black")) +
    theme(axis.title = element_blank(), axis.text = element_blank()) +
    scale_fill_distiller(palette = 'BuPu', name = 'neonic log sum')+
    ggtitle("")
  
  
  agri_pest_plot <- plot_grid(agri_plot, pest_plot)
  
  ggsave(agri_pest_plot, filename = paste0('other/neonic_log_sum/', state_codes[state_codes$STATE_FIPS_CODE == st,1], '.jpeg'))
  
}




############# now check at the actual sites ############



neonics_2014 <- neonics_site[YEAR == 2014]

neonics_site_year <- neonics_2014[, .(mean(EPEST_HIGH_KG, na.rm = TRUE), mean(EPEST_LOW_KG, na.rm = TRUE)), by = .(site, COMPOUND)]

colnames(neonics_site_year) <- c('site', 'compound', 'EPEST_HIGH_KG', 'EPEST_LOW_KG')

## check how much variation there is within a site, maybe its worth adding the pesticide per site instead of averaging?

neonics_2014[COMPOUND == "IMIDACLOPRID" & site == "s763"]

mean_epest <- neonics_2014[COMPOUND == "IMIDACLOPRID"][,.(mean(EPEST_LOW_KG), mean(EPEST_HIGH_KG), .N, sd(EPEST_LOW_KG)), by = .(site)] %>% 
  arrange(N) 

sum_epest <- neonics_2014[COMPOUND == "IMIDACLOPRID"][,.(sum(EPEST_LOW_KG), sum(EPEST_HIGH_KG), .N, sd(EPEST_LOW_KG)), by = .(site)] %>% 
  arrange(N) 

left_join(mean_epest, sum_epest, by = 'site') %>% 
  ggplot(aes(x = V1.x, y = V1.y, colour = factor(N.x))) + geom_point() +
  xlab("mean estimates imidacloprid low") + ylab("sum estimates imidacloprid low") +
  scale_color_discrete(name = "Number of Counties per site")




## continue with the sites (assuming we are calculating the mean of epest )

site_area <- readRDS(paste0("clean_data/sites/area_US_",resolution, ".rds"))


crop_units_area_df <- agriculture_county2 %>% 
  mutate(STATE_FIPS_CODE = as.numeric(STATEFP), 
         COUNTY_FIPS_CODE= as.numeric(COUNTYFP)) %>% 
  left_join(site_county) %>% 
  group_by(site) %>% 
  summarise(crop_units = sum(N)) %>% 
  left_join(site_area) %>% 
  mutate(crop_units_area = crop_units/area)  %>% 
  mutate(scaled_crop_units = scale(crop_units_area, center = FALSE)) %>% 
  as.data.table()

setkey(crop_units_area_df, site)
setkey(neonics_site_year, site)

neonic_agrilculture_site <- neonics_site_year[crop_units_area_df]

all_sites <- sites@data

neonic_agriculture_full <- neonic_agrilculture_site %>% 
  right_join(all_sites) %>% 
  mutate(EPEST_LOW_KG = ifelse(is.na(EPEST_LOW_KG), 0, EPEST_LOW_KG),
         EPEST_HIGH_KG = ifelse(is.na(EPEST_HIGH_KG), 0, EPEST_HIGH_KG), 
         crop_units_area = ifelse(is.na(crop_units_area), 0, crop_units_area),
         scaled_crop_units = ifelse(is.na(scaled_crop_units), 0, scaled_crop_units))

pest_agriculture_full <- neonic_agriculture_full %>% 
  mutate(logV1 = log(EPEST_HIGH_KG+1)) %>% 
  group_by(site, scaled_crop_units) %>% 
  summarise(pest_neonic = sum(logV1))

pest_agriculture_full %>% 
  filter(site != "s18") %>% 
  ggplot(aes(x = scaled_crop_units, y = pest_neonic)) + geom_point() +
  xlab("Number of 3km agriculture plots in site per area of site scaled") +
  ylab("Sum of log neonics (mean epest per site)")




