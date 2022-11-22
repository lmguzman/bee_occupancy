library(dplyr)
library(stringr)
source("R/src/initialize.R")

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




region_v <- c("West","Center","NorthEast", "SouthEast")
region <- "Center"
family <- "ALL"
year_range <- c(1995, 2015)
model_v <- c("ms_era_1_area", "ms_env_US_area", "ms_env_era_area")


## era
model <- "ms_era_1_area"

res <- readRDS(paste0("model_outputs/res_counties_", paste0(year_range, collapse = "_"), "_",model,"_", family, "_", region, ".rds"))

res.summary <- readRDS(paste0("model_outputs/res.summary_counties_", paste0(year_range, collapse = "_"), "_",model,"_", family, "_", region, ".rds"))

my.data <- readRDS(paste0("clean_data/data_prepared/my_data_era_genus_counties_", paste0(year_range, collapse = "_"),  "_", family, "_", region, ".rds"))


# env
model <- "ms_env_US_area"

res <- readRDS(paste0("model_outputs/res_counties_", paste0(year_range, collapse = "_"), "_",model,"_", family, "_", region, ".rds"))

res.summary <- readRDS(paste0("model_outputs/res.summary_counties_", paste0(year_range, collapse = "_"), "_",model,"_", family, "_", region, ".rds"))

my.data <- readRDS(paste0("clean_data/data_prepared/my_data_env_genus_counties_", paste0(year_range, collapse = "_"),  "_", family, "_", region, ".rds"))


# env era
model <- "ms_env_era_area_2"

res <- readRDS(paste0("model_outputs/env_era/res_counties_", paste0(year_range, collapse = "_"), "_",model,"_", family, "_", region, ".rds"))

res.summary <- readRDS(paste0("model_outputs/env_era/res.summary_counties_", paste0(year_range, collapse = "_"), "_",model,"_", family, "_", region, ".rds"))

my.data <- readRDS(paste0("clean_data/data_prepared/my_data_env_genus_counties_", paste0(year_range, collapse = "_"),  "_", family, "_", region, ".rds"))


# env era
model <- "ms_env_area_2"

res <- readRDS(paste0("model_outputs/env/res_counties_", paste0(year_range, collapse = "_"), "_",model,"_", family, "_", region, ".rds"))

res.summary <- readRDS(paste0("model_outputs/env/res.summary_counties_", paste0(year_range, collapse = "_"), "_",model,"_", family, "_", region, ".rds"))

my.data <- readRDS(paste0("clean_data/data_prepared/my_data_env_genus_counties_", paste0(year_range, collapse = "_"),  "_", family, "_", region, ".rds"))


vars <- rownames(res.summary$psrf$psrf)
summ <- get.summ(vars)

summ[str_detect(rownames(summ), 'p.era'),]

summ.paper <- summ[str_detect(rownames(summ), 'mu.psi'),]

summ.paper %>% View()



res.summary






####### Species trends ######

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



#### Check shuffle apidae 
library(ggplot2)

# env
res <- readRDS("model_outputs/res_US_100_1997_2016_ms_env_US_Apidaeshuffle.rds")

res.summary <- readRDS("model_outputs/res.summary_US_100_1997_2016_ms_env_US_Apidaeshuffle.rds")

my.data <- readRDS("clean_data/data_prepared/my_data_env_shuffle_US_100_1997_2016_Apidae.rds")

my.data[[1]]$prec %>% hist(xlab = "precipitation")

prec_all <- my.data[[1]]$prec %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column("site") %>% 
  pivot_longer(names_to = 'year', values_to = 'precipitation', -site)

prec_mean <- prec_all %>% group_by(year) %>% summarise(mean_prec = mean(precipitation))
  
ggplot(data = prec_all, aes(x = precipitation, fill = year)) +
  geom_histogram() +
  facet_wrap(~year) +
  geom_vline(data = prec_mean, aes(xintercept = mean_prec))



my.data[[1]]$agriculture %>% hist(xlab = "agriculture")

agriculture_all <- my.data[[1]]$agriculture %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column("site") %>% 
  pivot_longer(names_to = 'year', values_to = 'agriculture', -site)

agriculture_mean <- agriculture_all %>% group_by(year) %>% summarise(mean_v = mean(agriculture))

ggplot(data = agriculture_all, aes(x = agriculture, fill = year)) +
  geom_histogram() +
  facet_wrap(~year) +
  geom_vline(data = agriculture_mean, aes(xintercept = mean_v))



my.data[[1]]$drought %>% hist(xlab = "drought")

drought_all <- my.data[[1]]$drought %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column("site") %>% 
  pivot_longer(names_to = 'year', values_to = 'drought', -site)

drought_mean <-drought_all %>% group_by(year) %>% summarise(mean_v = mean(drought))

ggplot(data = drought_all, aes(x = drought, fill = year)) +
  geom_histogram() +
  facet_wrap(~year) +
  geom_vline(data = drought_mean, aes(xintercept = mean_v))



my.data[[1]]$floral %>% hist(xlab = "floral")

floral_all <- my.data[[1]]$floral %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column("site") %>% 
  pivot_longer(names_to = 'year', values_to = 'floral', -site)

floral_mean <-floral_all %>% group_by(year) %>% summarise(mean_v = mean(floral))

ggplot(data = floral_all, aes(x = floral, fill = year)) +
  geom_histogram() +
  facet_wrap(~year) +
  geom_vline(data = floral_mean, aes(xintercept = mean_v))



library(data.table)

all_obs <- fread("/Volumes/Rasters/USC/bee_occupancy/raw_data/observations/cleaned_contiguousUS_records.csv")

observations <- readRDS("clean_data/observations/observations_US_100.rds")

observations$finalName %>% unique() %>% length()

observations[finalName == species_decreasing$species[3], .N, .(year)] %>% 
  arrange(year)

all_obs[finalName == species_decreasing$species[1], .(year, finalLatitude, finalLongitude, recordedBy, stateProvince)][,.N, year] %>% 
  arrange()

