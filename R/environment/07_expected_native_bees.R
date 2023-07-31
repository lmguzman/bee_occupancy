#### script to create expected richness map for native bees ##

library(dplyr)
library(raster)
library(sf)

## for each family since the polygon data is saved per family

family_list <- c("Andrenidae","Apidae", "Colletidae", "Halictidae", "Megachilidae", "Melittidae")

# load counties

counties <- readRDS("clean_data/sites/sites_counties.RDS") 

for(fam in family_list){
 
  species_names <- list.files(paste0("bee_polygons/Species Grd Files (for nick)/", fam, " 30km/"), 
                              full.names = TRUE) %>% 
    str_remove("\\.gri|\\.grd") %>% unique()
  
  presence_matrix <- matrix(NA, nrow = nrow(counties), ncol = length(species_names))
  
  count <- 1
  for(sp in species_names){
    
    r <- raster(paste0(sp, ".grd"))
    
    counties_presence <- terra::extract(r, counties) 
    
    presence_matrix[,count] <- sapply(counties_presence, function(x) as.numeric(sum(x) > 0))
    
    count <- count + 1
  }
  
  saveRDS(presence_matrix, paste0("clean_data/native_expected/",fam,".rds"))
   
}


### put together all of the expected richnesses 

fam_files <- list.files("clean_data/native_expected/", full.names = TRUE)

all_expected_array <- matrix(NA, nrow = nrow(counties), ncol = length(fam_files))

for(f in 1:length(fam_files)){
  
  fam_mat <- readRDS(fam_files[f])
  
  all_expected_array[,f] <- rowSums(fam_mat)
  
}

expected_richness <- counties %>% 
  mutate(site = paste0("s_", state_county)) %>% 
  mutate(expected_richnness = rowSums(all_expected_array))

saveRDS(expected_richness, 'clean_data/native_expected/expected_richness.rds')
