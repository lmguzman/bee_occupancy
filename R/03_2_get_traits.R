library(dplyr)
library(tidyr)
library(stringr)

## read in the california trait data 
trait_cal <- read.csv("raw_data/traits/functional_traits - bee.csv")


### if the nest location is mixed -- we will put below for the purposes of pesticide exposure
nesting_genus <- trait_cal %>%
  dplyr::select(GenusSpecies, NestLoc) %>% 
  filter(NestLoc != "") %>% 
  dplyr::mutate(NestLoc = ifelse(NestLoc == 'mix', "below", NestLoc)) %>% 
  separate(GenusSpecies, c("Genus", "Species"), sep = " ") %>% 
  group_by(Genus, NestLoc) %>% 
  summarise(n = n()) %>% 
  filter(!Genus %in% c('Megachile', "Osmia")) %>% 
  ungroup() %>% 
  select(-n) %>% 
  rename(genus = Genus)

## get observations and the genus
observations_raw <- readRDS(paste0("clean_data/observations/observations_counties.rds"))

observations_raw$genus <- str_extract(observations_raw$finalName, "[A-Z][a-z]*")

genus_no_data <- observations_raw %>% 
  select(genus) %>% 
  unique() %>% 
  filter(!genus %in% nesting_genus$Genus) %>% 
  arrange(genus)

genus_no_data %>% View()

#### get nesting location for the other genera

nesting_loc <- read.csv('raw_data/traits/nesting_location - Sheet1.csv') %>% 
  dplyr::select(-notes) 


nesting_loc_genus <- bind_rows(nesting_genus, nesting_loc)

write.csv(nesting_loc_genus, 'clean_data/traits/nesting_location_genus.csv',row.names = FALSE)
