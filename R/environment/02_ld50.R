#### script to get LD 50 for honey bees from ECOTOX database 

library(dplyr)
library(stringr)
library(data.table)

#### Neonicotinoids LD50 ####

### read terrestrial organism data from the ECOTOX website

all_data_files <- list.files("raw_data/ecotox_report/", full.names = TRUE)

all_data_files <- list.files("/Volumes/Rasters/USC/bee_occupancy/raw_data/ecotox_report/", full.names = TRUE)

all_data_files <- all_data_files[str_detect(all_data_files, "Terrestrial")]

## compile results from the terrestrial ecotox data 

all_data_terrestrial <- list()

for(f in all_data_files){
  
  all_data_terrestrial[[f]] <- read.delim(f, sep = "|") %>% 
    mutate(aq_ter = "Terrestrial") %>% 
    as.data.table()
  
}

all_df_terrestrial <- rbindlist(all_data_terrestrial)

## clean column names

colnames_clean_terrestrial <- colnames(all_df_terrestrial) %>% 
  str_remove("X") %>% 
  str_replace_all("\\.\\.", "_") %>% 
  str_replace_all("\\.", "_") %>% 
  str_remove("^_") %>% 
  str_remove("_$")

colnames(all_df_terrestrial) <- colnames_clean_terrestrial

## select only needed columns

terrestrial_all <- all_df_terrestrial[, .(CAS_Number, Chemical_Name, Species_Scientific_Name, Organism_Lifestage, Organism_Age_Mean, Organism_Age_Mean_Op,
                                          Exposure_Type, Media_Type, Test_Location, Number_of_Doses, Observed_Duration_Days,
                                          Observed_Response_Mean, Observed_Response_Min, Observed_Response_Max, Observed_Response_Units, Effect, Endpoint, Response_Site, aq_ter, Title),]

## add clean chemical names

chemical_table <- data.frame(Chemical_Name = unique(terrestrial_all$Chemical_Name), Chemical_short = c("Dinotefuran", "Imidacloprid", "Imidaclothiz", "Nitenpyram", "Nithiazine", "Thiacloprid",                                                                                            "Thiamethoxam", "Clothianidin", "Acetamiprid"))

terrestrial_all <- terrestrial_all %>% 
  left_join(chemical_table)

## Filter only apis studies with end point ld50

apis_studies <- terrestrial_all %>% 
  filter(str_detect(Species_Scientific_Name, "Apis") & Endpoint == "LD50") %>% 
  mutate(Observed_Response_Mean = as.numeric(str_remove(Observed_Response_Mean, "\\/"))) %>% 
  filter(!is.na(Observed_Response_Mean))

## standardize units to ng/org

apis_studies_std <- apis_studies %>% 
  ## replace bee for org
  mutate(Observed_Response_Units = str_replace(Observed_Response_Units, "bee", "org")) %>% 
  ## change from ug to ng
  mutate(Observed_Response_Units_std = str_replace(Observed_Response_Units, "ug/org", "ng/org")) %>% 
  mutate(Observed_Response_Mean_std = ifelse(str_detect(Observed_Response_Units, "ug/org"), Observed_Response_Mean *1000, Observed_Response_Mean)) %>% 
  ## change from pg to ng
  mutate(Observed_Response_Units_std = str_replace(Observed_Response_Units_std, "pg/org", "ng/org")) %>% 
  mutate(Observed_Response_Mean_std = ifelse(str_detect(Observed_Response_Units, "pg/org"), Observed_Response_Mean *0.001, Observed_Response_Mean_std)) %>% 
  ## change from mg to ng
  mutate(Observed_Response_Units_std = str_replace(Observed_Response_Units_std, "mg/org", "ng/org")) %>% 
  mutate(Observed_Response_Mean_std = ifelse(str_detect(Observed_Response_Units, "mg/org"), Observed_Response_Mean *1000000, Observed_Response_Mean_std)) 
  
## only keep studies with the same units

apis_studies <- apis_studies_std %>% 
  filter(Observed_Response_Units_std %in% c("ng/org", "AI ng/org"))

## clean study duration 

apis_studies_duration <- apis_studies %>% 
  mutate(Observed_Duration_Days = round(as.numeric(Observed_Duration_Days))) %>% 
  filter(Observed_Duration_Days < 4)

## filter only apis mellifera and summarise LD50 for dermal and food

apis_ld50_mean<- apis_studies_duration %>% 
  filter(Species_Scientific_Name == "Apis mellifera") %>% 
  ## seems outlier based on other results
  group_by(Chemical_short, Exposure_Type) %>% 
  summarise(mean_ld50 = mean(Observed_Response_Mean_std), median_ld50 = median(Observed_Response_Mean_std), sd_ld50 = sd(Observed_Response_Mean_std), n = n()) %>% 
  filter(Exposure_Type %in% c('Dermal', "Food")) 

write.csv(apis_ld50_mean, "clean_data/pesticide/apis_ld50_mean.csv", row.names = FALSE)


#### pyrethroids LD50 ####

## load and compile data 

all_data_files <- list.files("/Volumes/Rasters/USC/bee_occupancy/raw_data/ecotox_report/pyrethroids/", full.names = TRUE)

all_data_terrestrial <- list()

for(f in all_data_files){
  
  all_data_terrestrial[[f]] <- read.delim(f, sep = "|") %>% 
    mutate(aq_ter = "Terrestrial") %>% 
    as.data.table()
  
}

all_df_terrestrial <- rbindlist(all_data_terrestrial)

## clean column names

colnames_clean_terrestrial <- colnames(all_df_terrestrial) %>% 
  str_remove("X") %>% 
  str_replace_all("\\.\\.", "_") %>% 
  str_replace_all("\\.", "_") %>% 
  str_remove("^_") %>% 
  str_remove("_$")

colnames(all_df_terrestrial) <- colnames_clean_terrestrial

## select only needed columns

terrestrial_all <- all_df_terrestrial[, .(CAS_Number, Chemical_Name, Species_Scientific_Name, Organism_Lifestage, Organism_Age_Mean, Organism_Age_Mean_Op,
                                          Exposure_Type, Media_Type, Test_Location, Number_of_Doses, Observed_Duration_Days,
                                          Observed_Response_Mean, Observed_Response_Min, Observed_Response_Max, Observed_Response_Units, Effect, Endpoint, Response_Site, aq_ter, Title),]

## add clean chemical names based on cas numbers

cas_numbers <- read.csv("/Volumes/Rasters/USC/bee_occupancy/raw_data/ecotox_report/CAS_compunds.csv")

terrestrial_all <- terrestrial_all %>% 
  left_join(cas_numbers, by = c("CAS_Number" = 'CAS'))

## Filter only apis studies with end point ld50

apis_studies <- terrestrial_all %>% 
  filter(str_detect(Species_Scientific_Name, "Apis") & Endpoint == "LD50") %>% 
  mutate(Observed_Response_Mean = as.numeric(str_remove(Observed_Response_Mean, "\\/"))) %>% 
  filter(!is.na(Observed_Response_Mean))

## standardize units to ng/org

apis_studies_std <- apis_studies %>% 
  ## replace bee for org
  mutate(Observed_Response_Units = str_replace(Observed_Response_Units, "bee", "org")) %>% 
  ## change from ug to ng
  mutate(Observed_Response_Units_std = str_replace(Observed_Response_Units, "ug/org", "ng/org")) %>% 
  mutate(Observed_Response_Mean_std = ifelse(str_detect(Observed_Response_Units, "ug/org"), Observed_Response_Mean *1000, Observed_Response_Mean)) %>% 
  ## change from pg to ng
  mutate(Observed_Response_Units_std = str_replace(Observed_Response_Units_std, "pg/org", "ng/org")) %>% 
  mutate(Observed_Response_Mean_std = ifelse(str_detect(Observed_Response_Units, "pg/org"), Observed_Response_Mean *0.001, Observed_Response_Mean_std)) %>% 
  ## change from mg to ng
  mutate(Observed_Response_Units_std = str_replace(Observed_Response_Units_std, "mg/org", "ng/org")) %>% 
  mutate(Observed_Response_Mean_std = ifelse(str_detect(Observed_Response_Units, "mg/org"), Observed_Response_Mean *1000000, Observed_Response_Mean_std)) 

## only keep studies with the same units

apis_studies <- apis_studies_std %>% 
  filter(Observed_Response_Units_std %in% c("ng/org", "AI ng/org"))

apis_studies_duration <- apis_studies %>% 
  mutate(Observed_Duration_Days = round(as.numeric(Observed_Duration_Days))) %>% 
  filter(Observed_Duration_Days < 4)

## filter only apis mellifera and summarise LD50 for dermal and food

apis_ld50_mean<- apis_studies_duration %>% 
  filter(Species_Scientific_Name == "Apis mellifera") %>% 
  mutate(Exposure_Type_simple = case_when(Exposure_Type %in% c("Diet, unspecified", "Food") ~ "Food",
                                          Exposure_Type %in% c("Dermal", "Topical, general") ~ "Dermal",
                                          TRUE ~ Exposure_Type)) %>% 
  ## seems outlier based on other results
  group_by(Compound, Exposure_Type_simple) %>% 
  summarise(mean_ld50 = mean(Observed_Response_Mean_std), median_ld50 = median(Observed_Response_Mean_std), sd_ld50 = sd(Observed_Response_Mean_std), n = n()) %>% 
  filter(Exposure_Type_simple %in% c('Dermal', "Food")) 


write.csv(apis_ld50_mean, "clean_data/pesticide/apis_ld50_mean_pyrethoid.csv", row.names = FALSE)


####### other compounds ####

## load and compile data 

all_data_files <- list.files("/Volumes/Rasters/USC/bee_occupancy/raw_data/ecotox_report/other/", full.names = TRUE)

all_data_files <- all_data_files[str_detect(all_data_files, "Terrestrial")]

all_data_terrestrial <- list()

for(f in all_data_files){
  
  all_data_terrestrial[[f]] <- read.delim(f, sep = "|") %>% 
    mutate(aq_ter = "Terrestrial") %>% 
    as.data.table()
  
}

all_df_terrestrial <- rbindlist(all_data_terrestrial)

## clean column names

colnames_clean_terrestrial <- colnames(all_df_terrestrial) %>% 
  str_remove("X") %>% 
  str_replace_all("\\.\\.", "_") %>% 
  str_replace_all("\\.", "_") %>% 
  str_remove("^_") %>% 
  str_remove("_$")

colnames(all_df_terrestrial) <- colnames_clean_terrestrial

## select only needed columns

terrestrial_all <- all_df_terrestrial[, .(CAS_Number, Chemical_Name, Species_Scientific_Name, Organism_Lifestage, Organism_Age_Mean, Organism_Age_Mean_Op,
                                          Exposure_Type, Media_Type, Test_Location, Number_of_Doses, Observed_Duration_Days,
                                          Observed_Response_Mean, Observed_Response_Min, Observed_Response_Max, Observed_Response_Units, Effect, Endpoint, Response_Site, aq_ter, Title),]

## add clean chemical names based on cas numbers

cas_numbers <- read.csv("/Volumes/Rasters/USC/bee_occupancy/raw_data/ecotox_report/other/CAS_compunds.csv")

terrestrial_all <- terrestrial_all %>% 
  left_join(cas_numbers, by = c("CAS_Number" = 'CAS')) 

## Filter only apis studies with end point ld50

apis_studies <- terrestrial_all %>% 
  filter(str_detect(Species_Scientific_Name, "Apis") & Endpoint == "LD50") %>% 
  mutate(Observed_Response_Mean = as.numeric(str_remove(Observed_Response_Mean, "\\/"))) %>% 
  filter(!is.na(Observed_Response_Mean))

## standardize units to ng/org

apis_studies_std <- apis_studies %>% 
  ## replace bee for org
  mutate(Observed_Response_Units = str_replace(Observed_Response_Units, "bee", "org")) %>% 
  ## change from ug to ng
  mutate(Observed_Response_Units_std = str_replace(Observed_Response_Units, "ug/org", "ng/org")) %>% 
  mutate(Observed_Response_Mean_std = ifelse(str_detect(Observed_Response_Units, "ug/org"), Observed_Response_Mean *1000, Observed_Response_Mean)) %>% 
  ## change from pg to ng
  mutate(Observed_Response_Units_std = str_replace(Observed_Response_Units_std, "pg/org", "ng/org")) %>% 
  mutate(Observed_Response_Mean_std = ifelse(str_detect(Observed_Response_Units, "pg/org"), Observed_Response_Mean *0.001, Observed_Response_Mean_std)) %>% 
  ## change from mg to ng
  mutate(Observed_Response_Units_std = str_replace(Observed_Response_Units_std, "mg/org", "ng/org")) %>% 
  mutate(Observed_Response_Mean_std = ifelse(str_detect(Observed_Response_Units, "mg/org"), Observed_Response_Mean *1000000, Observed_Response_Mean_std)) 

## only keep studies with the same units

apis_studies <- apis_studies_std %>% 
  filter(Observed_Response_Units_std %in% c("ng/org", "AI ng/org"))

apis_studies_duration <- apis_studies %>% 
  mutate(Observed_Duration_Days = round(as.numeric(Observed_Duration_Days))) %>% 
  filter(Observed_Duration_Days < 4)

## filter only apis mellifera and summarise LD50 for dermal and food

apis_ld50_mean<- apis_studies_duration %>% 
  filter(Species_Scientific_Name == "Apis mellifera") %>% 
  mutate(Exposure_Type_simple = case_when(Exposure_Type %in% c("Diet, unspecified", "Food") ~ "Food",
                                          Exposure_Type %in% c("Dermal", "Topical, general") ~ "Dermal",
                                          TRUE ~ Exposure_Type)) %>% 
  ## seems outlier based on other results
  group_by(Compound, Exposure_Type_simple) %>% 
  summarise(mean_ld50 = mean(Observed_Response_Mean_std), median_ld50 = median(Observed_Response_Mean_std), sd_ld50 = sd(Observed_Response_Mean_std), n = n()) %>% 
  filter(Exposure_Type_simple %in% c('Dermal', "Food")) 


apis_ld50_mean %>% 
  dplyr::select(Compound, Exposure_Type_simple, mean_ld50) %>% 
  pivot_wider(names_from='Exposure_Type_simple', values_from = 'mean_ld50')

write.csv(apis_ld50_mean, "clean_data/pesticide/apis_ld50_mean_other.csv", row.names = FALSE)

