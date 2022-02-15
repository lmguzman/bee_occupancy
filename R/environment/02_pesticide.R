library(dplyr)
library(data.table)
library(sf)
library(rgeos)
library(ggplot2)
library(cowplot)
library(tidyr)

### pesticides of interest for bees 
#"IMIDACLOPRID" 
#Osmia  https://www.pnas.org/content/118/48/e2109909118.full#ref-37
# Apis M https://link.springer.com/article/10.1007/s10646-010-0566-0 (meta analysis)
# Bombus Terrestris https://science.sciencemag.org/content/336/6079/351.abstract 
# Bombus Terrestris https://www.nature.com/articles/nature11585
#"THIAMETHOXAM"
#Apis M  https://www.science.org/doi/abs/10.1126/science.1215039

### EPA
## LD50 for highly toxic for honey bees https://www.mda.state.mn.us/protecting/bmps/pollinators/beetoxicity
## neonicotinoids as a whole category

#### highly toxic for honey bees
# ABAMECTIN, CARBARYL, OXAMYL, PYRIDABEN, ACEPHATE, CHLORETHOXYFOS, CHLORPYRIFOS, DIAZINON, DIMETHOATE, MALATHION, FIPRONIL, PYRETHRIN, SULFOXAFLOR

##### Pyrethroid
## ALPHA-CYPERMETHRIN, BETA-CYFLUTHRIN, BIFENTHRIN, CYFLUTHRIN, DELTAMETHRIN, ESFENVALERATE, GAMMA-CYHALOTHRIN, LAMBDA-CYHALOTHRIN, PERMETHRIN, TEFLUTHRIN, ZETA-CYPERMETHRIN

##### neonicotinoids
# ACETAMIPRID, CLOTHIANIDIN, DINOTEFURAN, IMIDACLOPRID, THIAMETHOXAM




## download county level pesticide data from https://water.usgs.gov/nawqa/pnsp/usage/maps/county-level/


input_dir <- "/Volumes/Rasters/USC/bee_occupancy/raw_data/pesticide/pesticide_US/"
files_dir <- list.files(input_dir)

## create list of pesticides of interest -- compound names from above website

gen_toxic <- list()

gen_toxic_compounds <- c("ABAMECTIN", "CARBARYL", "OXAMYL", "PYRIDABEN", "ACEPHATE", 
                         "CHLORETHOXYFOS", "CHLORPYRIFOS", "DIAZINON", "DIMETHOATE", 
                         "MALATHION", "FIPRONIL", "SULFOXAFLOR")

pyrethroid <- list()

pyrethroid_compunds <- c("ALPHA CYPERMETHRIN", "BIFENTHRIN", "CYFLUTHRIN", 
                         "DELTAMETHRIN", "ESFENVALERATE", "CYHALOTHRIN-GAMMA", "CYHALOTHRIN-LAMBDA", 
                         "PERMETHRIN", "TEFLUTHRIN", "ZETA-CYPERMETHRIN")

neonics <- list()

neonic_compounds <- c("ACETAMIPRID", "CLOTHIANIDIN", "DINOTEFURAN", "IMIDACLOPRID", "THIAMETHOXAM")


### for each year extract compounds of interest 

for(i in 1:length(files_dir)){
  
  # read files
  pesticide_data <- fread(paste0(input_dir,files_dir[i]))
  
  ## extract generic organophospahes
  
  gen_toxic[[i]] <- pesticide_data[COMPOUND %in% gen_toxic_compounds]
  
  ## extract pyrethroid
  
  pyrethroid[[i]] <- pesticide_data[COMPOUND %in% pyrethroid_compunds]
  
  ## extract neonicotinoid
  
  neonics[[i]] <- pesticide_data[COMPOUND %in% neonic_compounds]
  
}

gen_toxic_all <- rbindlist(gen_toxic)

pyrethroid_all <- rbindlist(pyrethroid)

neonics_all <- rbindlist(neonics)


saveRDS(gen_toxic_all, "clean_data/pesticide/gen_toxic_county.rds")
saveRDS(pyrethroid_all, "clean_data/pesticide/pyrethroid_county.rds")
saveRDS(neonics_all, "clean_data/pesticide/neonics_county.rds")

