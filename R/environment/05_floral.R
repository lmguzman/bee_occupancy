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


## load the Koh bee suitability values ##

floral_suitability <- read.csv("/Volumes/Rasters/USC/bee_occupancy/raw_data/land_use/koh_data/Floral resource priors-Table 1.csv")

nesting_suitability <- read.csv("/Volumes/Rasters/USC/bee_occupancy/raw_data/land_use/koh_data/Nesting resource priors-Table 1.csv")

## load crop data layer for 2008##

crop_cover <- raster('/Volumes/Rasters/USC/bee_occupancy/raw_data/land_use/crop_data_layer/2008_30m_cdls/2008_30m_cdls.img')

## load original crop categories (from the CDL) ##

crop_cat_orig <- read.csv('/Volumes/Rasters/USC/bee_occupancy/raw_data/land_use/crop_data_layer/land_cover_categories.csv')

## load Koh crop categories

crop_cat_koh <- read.csv('/Volumes/Rasters/USC/bee_occupancy/raw_data/land_use/koh_data/crop_categories_koh.csv')

## load hyde data for 2008 ##

hyde <- r <- raster("/Volumes/Rasters/USC/bee_occupancy/raw_data/land_use/hyde/anthromes2008AD.asc")

### anthronome classes for hyde

anthronome_id <- data.frame(class_number = c(11, 12, 21, 22, 23, 24, 31, 32, 33, 34, 41, 42, 43, 51, 52, 53, 54, 61, 62, 
                                             63, 60), class_name = c("11 Urban", "12 Dense settlements", "21 Village, Rice", "22 Village, Irrigated",
                                                                     "23 Village, Rainfed", "24 Village, Pastoral", "31 Croplands, residential irrigated",
                                                                     "32 Croplands, residential rainfed", "33 Croplands, populated", "34 Croplands, pastoral",
                                                                     "41 Rangeland, residential","42 Rangeland, populated","43 Rangeland, remote",
                                                                     "51 Semi-natural woodlands, residential","52 Semi-natural woodlands, populated",
                                                                     "53 Semi-natural woodlands, remote","54 Semi-natural treeless and barren lands",
                                                                     "61 Wild, remote - woodlands","62 Wild, remote - treeless & barren","63 Wild, remote - ice",
                                                                     "70 No definition"))



### join the original crop table with the koh crop table ###

crop_cat_orig2 <- crop_cat_orig %>% 
  mutate(Land_Cover_simplified = ifelse(str_detect(Land_Cover, "Dbl Crop"), "Dbl Crop", Land_Cover)) %>% 
  mutate(Land_Cover_simplified = ifelse(Land_Cover %in% c("Grassland Herbaceous"), "Grassland/Herbaceous", Land_Cover_simplified)) %>% 
  mutate(Land_Cover_simplified = ifelse(Land_Cover %in% c("Peanuts"), "Peanut", Land_Cover_simplified)) %>% 
  mutate(Land_Cover_simplified = ifelse(Land_Cover %in% c("Misc Vegs & Fruits"), "Miscellaneous Vegetables and Fruits", Land_Cover_simplified))

original_koh <- data.frame()

for(i in 1:length(crop_cat_orig2$Land_Cover)){
  
  new_numbers <- unlist(dplyr::select(filter(crop_cat_koh, str_detect(Original_categories_CDL, crop_cat_orig2$Land_Cover_simplified[i])), No_crop_re))
  
  if(crop_cat_orig2$Land_Cover_simplified[i] == "Dbl Crop"){
    new_numbers_2 <- as.character(new_numbers[2])
  }else if(crop_cat_orig2$Land_Cover_simplified[i] == "Lettuce"){
    new_numbers_2 <- as.character(new_numbers[2])
  }else if(crop_cat_orig2$Land_Cover_simplified[i] %in% c('Corn', 'Cotton', 'Sorghum', 'Soybeans', 
                                                          'Barley', 'Durum Wheat', 'Winter Wheat',
                                                          'Oats', 'Alfalfa', 'Peas', 'Broccoli', 'Pears',
                                                          'Cucumbers'))
    new_numbers_2 <- as.character(new_numbers[1])
  else{
    new_numbers_2 <- paste(new_numbers, collapse = ',')
  }
  
  new_matches <- data.frame(Land_cover_orig = crop_cat_orig2$Land_Cover[i], koh_cats = new_numbers_2)
  
  original_koh <- bind_rows(original_koh, new_matches)
  
}

## creates a directory between the crop data layer and the Koh et al categories 

CDL_Koh_directory <- crop_cat_orig2 %>% 
  left_join(original_koh, by = c('Land_Cover' = 'Land_cover_orig')) %>% 
  filter(!(nchar(koh_cats) <1 | nchar(koh_cats) >2)) %>% 
  dplyr::select(-Land_Cover_simplified) %>% 
  mutate(koh_cats = as.numeric(koh_cats)) %>% 
  left_join(dplyr::select(crop_cat_koh, No_crop_re, Reclassified_categories), by = c('koh_cats' = 'No_crop_re')) %>% 
  rename(CDL_categories = Categorization_Code, CDL_land_cover = Land_Cover, 
         Koh_categories = koh_cats, Koh_land_cover = Reclassified_categories) %>% 
  unique() %>% 
  data.table()


###### join the hyde table with the crop data layer####

# extract all of the North America values for the hyde database 

# cut hyde by the North America box
dd.box <- bbox2SP(n=50,
                  e=-50,
                  s=20,
                  w=-150)

hyde_na <- raster::intersect(hyde, dd.box)

plot(hyde_na)

# extract all of the cell values

hyde_vals <- raster::extract(hyde_na, 1:ncell(hyde_na))

# bind hyde values with coordinates

hyde_table <- data.table(cbind(anthronome_cat = hyde_vals, coordinates(hyde_na)))

hyde_table_coord <- hyde_table[!is.na(anthronome_cat)]

## extract the coordinate values for the CDL map ##

# change the projection of the hyde points

hyde_coordinates <- SpatialPoints(hyde_table_coord[,.(x,y)])
projection(hyde_coordinates) <- CRS("+init=epsg:4326")

proj3 <- "+proj=aea +lat_0=40 +lon_0=-96 +lat_1=20 +lat_2=60 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"

hyde_coordinates_transform <- spTransform(hyde_coordinates, CRS(proj3))

# extract CDL values and add the to the hyde table 

crop_values <- raster::extract(crop_cover, hyde_coordinates_transform)

hyde_table_coord[, 'CDL_cat':= crop_values]

hyde_CDL_table <- hyde_table_coord[!is.na(CDL_cat)]


#### calculate suitability #####

# join hyde and CDL_koh directory

setkey(hyde_CDL_table, CDL_cat)

setkey(CDL_Koh_directory, CDL_categories)

hyde_CDL_KOH <- hyde_CDL_table[CDL_Koh_directory]

## clean suitability 
clean_floral_suitability <- floral_suitability %>% 
  filter(!Categories %in% c("Grassland Herbaceous", "Pasture/Hay", "Other hay", 
                            "Wetlands")) %>% 
  dplyr::select(Categories, Spring.mean, Summer.mean, Autumn.mean) %>% 
  mutate(Categories = case_when(Categories == "Beans" ~ "Bean",
                                Categories == "Corns" ~ "Corn",
                                Categories == "Dbl Crop" ~ "Double (Dbl) Crop",
                                Categories == "Grains" ~ "Grain",
                                Categories == "Grassland/Pasture" ~ "Grass/Pasture",
                                Categories == "Idle Crops" ~ "Idle Cropland",
                                Categories == "Root Vegs" ~ "Root Vegetables",
                                Categories == "Vegs" ~ "Vegetables",
                                Categories == "Vegs & Fruits" ~ "Vegetables and Fruits",
                                TRUE ~ Categories)) %>% 
  data.table()


clean_nesting_suitability <- nesting_suitability %>% 
  filter(!Categories %in% c("Grassland Herbaceous", "Pasture/Hay", "Other hay", 
                            "Wetlands")) %>% 
  dplyr::select(Categories, Ground.mean, Cavity.mean, Stem.mean, Wood.mean) %>% 
  mutate(Categories = case_when(Categories == "Beans" ~ "Bean",
                                Categories == "Corns" ~ "Corn",
                                Categories == "Dbl Crop" ~ "Double (Dbl) Crop",
                                Categories == "Grains" ~ "Grain",
                                Categories == "Grassland/Pasture" ~ "Grass/Pasture",
                                Categories == "Idle Crops" ~ "Idle Cropland",
                                Categories == "Root Vegs" ~ "Root Vegetables",
                                Categories == "Vegs" ~ "Vegetables",
                                Categories == "Vegs & Fruits" ~ "Vegetables and Fruits",
                                TRUE ~ Categories)) %>% 
  data.table()
## join hyde_CDL_KOH with suitability

setkey(hyde_CDL_KOH, Koh_land_cover)
setkey(clean_floral_suitability, Categories)
setkey(clean_nesting_suitability, Categories)

hyde_CDL_KOH_suit <- hyde_CDL_KOH[clean_floral_suitability]

hyde_CDL_KOH_suit_all <- hyde_CDL_KOH_suit[clean_nesting_suitability]

saveRDS(hyde_CDL_KOH_suit_all, file = 'clean_data/land_use/hyde_CDL_KOH_suitability_2008.rds')


#### check floral and nesting suitability change through time #####

hyde_suit <- hyde_CDL_KOH_suit_all[!is.na(anthronome_cat), 
                                   .("spring_mean" = mean(Spring.mean), 
                                     "summer_mean" = mean(Summer.mean),
                                     "fall_mean" = mean(Autumn.mean),
                                     "ground_mean" = mean(Ground.mean),
                                     "cavity_mean" = mean(Cavity.mean),
                                     "stem_mean" = mean(Stem.mean),
                                     "wood_mean" = mean(Wood.mean)
                                   ),
                                   by = (anthronome_cat)]

hyde_suit[, 'floral_all':=(spring_mean+summer_mean+fall_mean)]
hyde_suit[, 'nesting_all':=(ground_mean+cavity_mean+stem_mean +wood_mean)]

years <- c(seq(1900, 2000, 10), seq(2001, 2017, 1))

dd.box <- bbox2SP(n=87,
                  e=-40,
                  s=12,
                  w=-190)


all_land_vars <- list()

count <- 1
for(yr in years){
  
  #load hyde raster
  
  hyde <- raster(paste0("/Volumes/Rasters/USC/bee_occupancy/raw_data/land_use/hyde/anthromes",yr,"AD.asc"))
  
  # cut hyde raster to be in NA
  
  hyde_na <- raster::intersect(hyde, dd.box)
  
  # extract all of the values of hyde raster
  
  hyde_vals <- raster::extract(hyde_na, 1:ncell(hyde_na))
  
  # bind hyde values with coordinates
  
  hyde_table <- data.table(cbind(anthronome_cat = hyde_vals, coordinates(hyde_na)))
  
  # remove NA values
  
  hyde_table_coord_raw <- hyde_table[!is.na(anthronome_cat)]
  
  # join the coordinates with suitability values
  setkey(hyde_table_coord_raw, anthronome_cat)
  setkey(hyde_suit, anthronome_cat)
  
  coord_suits <- hyde_table_coord_raw[hyde_suit]
  
  land_use_cats <- c('spring_mean', 'summer_mean', 'fall_mean', 'ground_mean',
                     'cavity_mean', 'stem_mean',  'wood_mean', 'floral_all', 'nesting_all')
  
  current_list <- list()
  
  lcount <- 1
  
  for(l in land_use_cats){
    
    cols <- c('x', 'y', l)
    
    selected_var <- coord_suits[,..cols]
  
    selected_var[,'year':= yr]
    selected_var[,'suitability':= l]
    
    current_list[[lcount]] <- selected_var
    
    lcount <- lcount + 1
  }
  
  all_land_vars[[count]] <- current_list
  
  count <- count + 1
}


land_use_cats <- paste0(c('spring_mean', 'summer_mean', 'fall_mean', 'ground_mean',
                          'cavity_mean', 'stem_mean',  'wood_mean', 'floral_all', 'nesting_all'), "_all")

for(i in 1:length(land_use_cats)){
  
  temp_df <- rbindlist(lapply(all_land_vars, FUN = function(x) x[[i]]))
  
  saveRDS(temp_df, file = paste0('raw_data/land_use/',land_use_cats[i],'.rds'))
  
}



### get suitability values fore each country and resolution ###

land_use_cats <- paste0(c('spring_mean', 'summer_mean', 'fall_mean', 'ground_mean',
                          'cavity_mean', 'stem_mean',  'wood_mean', 'floral_all', 'nesting_all'), "_all")


flor_nest_sites <- function(countries, resolution){
  
  sites <- readRDS(paste0("clean_data/sites/sites_",countries,"_",resolution, ".rds"))
  
  for(i in land_use_cats){
    
    temp_df <- readRDS(paste0('raw_data/land_use/', i,'.rds'))
    
    lat_lon <- temp_df[, .(x,y)]
    
    coordinates(lat_lon) <- ~ x + y
    
    proj4string(lat_lon) <- proj4string(sites)
    
    # get site number for each observation
    
    mean_all_df_site <- temp_df[, site := over(lat_lon, sites)]
    
    var_one <- c(str_remove(i, "_all"))
    
    expr <- parse(text = paste0('mean(', var_one,')'))
    
    suitability <- mean_all_df_site[!is.na(site), eval(expr), by = .(site, year)]
    
    colnames(suitability)[3] <- var_one
    
    saveRDS(suitability, paste0('clean_data/land_use/land_use_',countries,'_',resolution, "_",var_one,'.rds'))  
    
  }
}

### floral and nesting suitability for the US 

## assign sites for 100km resolution

flor_nest_sites("US", 100)

## assign sites for 50km resolution

flor_nest_sites("US", 50)


### floral and nesting suitability for the all NA

## assign sites for 100km resolution

flor_nest_sites("all", 100)

## assign sites for 50km resolution

flor_nest_sites("all", 50)



