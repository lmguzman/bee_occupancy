library(dplyr)
library(stringr)
source("R/src/initialize.R")

## era
res <- readRDS("model_outputs/res_US_100_1997_2016_ms_era_1.rds")

res.summary <- readRDS("model_outputs/res.summary_US_100_1997_2016_ms_era_1.rds")

my.data <- readRDS("clean_data/data_prepared/my_data_era_genus_US_100_1997_2016_ALL.rds")

## Apidae vp 

family_load <- "Apidae"

## era
res <- readRDS(paste0("model_outputs/res_US_100_1997_2016_ms_era_1_vp_",family_load,".rds"))

res.summary <- readRDS(paste0("model_outputs/res.summary_US_100_1997_2016_ms_era_1_vp_",family_load,".rds"))




family_load <- "Apidae"

## era
# res <- readRDS(paste0("model_outputs/res_US_100_1997_2016_ms_era_1_",family_load,".rds"))
# 
# res.summary <- readRDS(paste0("model_outputs/res.summary_US_100_1997_2016_ms_era_1_",family_load,".rds"))
# 
# my.data <- readRDS(paste0("clean_data/data_prepared/my_data_era_genus_US_100_1997_2016_",family_load,".rds"))


# env
res <- readRDS(paste0("model_outputs/res_US_100_1997_2016_ms_env_US_",family_load,".rds"))

res.summary <- readRDS(paste0("model_outputs/res.summary_US_100_1997_2016_ms_env_US_",family_load,".rds"))

my.data <- readRDS(paste0("clean_data/data_prepared/my_data_env_genus_US_100_1997_2016_",family_load,".rds"))



# env
res <- readRDS(paste0("model_outputs/res_US_100_1997_2016_ms_env_era_",family_load,".rds"))

res.summary <- readRDS(paste0("model_outputs/res.summary_US_100_1997_2016_ms_env_era_",family_load,".rds"))

my.data <- readRDS(paste0("clean_data/data_prepared/my_data_env_genus_US_100_1997_2016_",family_load,".rds"))



# env
res <- readRDS("model_outputs/res_US_100_1997_2016_ms_env_US_Apidaeshuffle.rds")

res.summary <- readRDS("model_outputs/res.summary_US_100_1997_2016_ms_env_US_Apidaeshuffle.rds")

my.data <- readRDS("clean_data/data_prepared/my_data_env_shuffle_US_100_1997_2016_Apidae.rds")

## check main trends

get.summ <- function(pars) {
  summ <- round(cbind(
    res.summary$summary$statistics[pars,'Mean',drop=FALSE],
    res.summary$summary$quantiles[pars,c('2.5%', '97.5%'),drop=FALSE],
    Rhat=res.summary$psrf$psrf[pars,1]
  ), digits=3)
  colnames(summ)[1] <- 'mean'
  summ
}


vars <- rownames(res.summary$psrf$psrf)
summ <- get.summ(vars)

summ.paper <- summ[str_detect(rownames(summ), 'mu.psi'),]

summ.paper %>% View()







species_trends <- cbind(data.frame(summ[str_detect(rownames(summ), 'psi.era'),]), species = my.data$sp)

species_increasing <- species_trends %>% 
  mutate(sig = ifelse((`X2.5.` < 0 & `X97.5.`< 0) | (`X2.5.` > 0 & `X97.5.`> 0), TRUE, FALSE)) %>% 
  filter(sig == TRUE & mean >0) %>% 
  arrange(desc(mean)) 

species_increasing %>% View()

species_decreasing <- species_trends %>% 
  mutate(sig = ifelse((`X2.5.` < 0 & `X97.5.`< 0) | (`X2.5.` > 0 & `X97.5.`> 0), TRUE, FALSE)) %>% 
  filter(sig == TRUE & mean <0) %>% 
  arrange(mean)

species_decreasing %>% View()

species_not_significantly <- species_trends %>% 
  mutate(sig = ifelse((`X2.5.` < 0 & `X97.5.`< 0) | (`X2.5.` > 0 & `X97.5.`> 0), TRUE, FALSE)) %>% 
  filter(sig == FALSE) %>% 
  arrange(desc(mean)) 

write.csv(species_not_significantly, "~/Downloads/species_model.csv")


species_intercepts <- cbind(data.frame(summ[str_detect(rownames(summ), 'psi.sp'),]), species = my.data$sp)

species_intercepts %>% 
  filter(species == "Bombus occidentalis")

species_trends %>% 
  filter(species == "Bombus occidentalis")

expit(-0.264 + -0.375)

expit(-0.264 + -0.375 + -0.106*10)





library(data.table)

all_obs <- fread("/Volumes/Rasters/USC/bee_occupancy/raw_data/observations/cleaned_contiguousUS_records.csv")

observations <- readRDS("clean_data/observations/observations_US_100.rds")

observations$finalName %>% unique() %>% length()

observations[finalName == species_decreasing$species[3], .N, .(year)] %>% 
  arrange(year)

all_obs[finalName == species_decreasing$species[1], .(year, finalLatitude, finalLongitude, recordedBy, stateProvince)][,.N, year] %>% 
  arrange()

