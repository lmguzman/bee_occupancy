library(ggplot2)
library(cowplot)
library(tidyr)
library(dplyr)
library(RColorBrewer)
library(stringr)
library(data.table)
library(purrr)
source("R/src/initialize.R")
source("R/src/plot_ocu_env_functions.R")


create_data_for_plots <- function(region){
  
  family <- "ALL"
  year_range <- c(1995, 2015)
  
  # env era
  model <- "ms_env_area_2"
  
  res <- readRDS(paste0("model_outputs/env/res_counties_", paste0(year_range, collapse = "_"), "_",model,"_", family, "_", region, ".rds"))
  
  res.summary <- readRDS(paste0("model_outputs/env/res.summary_counties_", paste0(year_range, collapse = "_"), "_",model,"_", family, "_", region, ".rds"))
  
  my.data <- readRDS(paste0("clean_data/data_prepared/my_data_env_genus_counties_", paste0(year_range, collapse = "_"),  "_", family, "_", region, ".rds"))
  
  ### assign correct chains ##
  
  sims.mat <- do.call(rbind, res$mcmc)
  sims.arr <-
    aperm(sapply(res$mcmc, I, simplify='array'), c(1,3,2))
  
  nera <- my.data[[1]]$nyr
  nsp <- my.data[[1]]$nsp
  species_directory <- my.data[[2]] %>% 
    mutate(sp_n = 1:n())
  
  
  
  # Get occupancy for every species only in year 7 (after neonics have been applied)
  
  sp_yr <- expand.grid(yr = 7, sp = 1:nsp)
  
  main_occupancy <- lapply(1:nrow(sp_yr), FUN = function(x) get.y.val.all(sims.mat = sims.mat, ss = sp_yr$sp[x], my.data = my.data,
                                                                          yr = sp_yr$yr[x], species_directory = species_directory, region = NA))
  
  env_main_occupancy <- rbindlist(main_occupancy)
  
  saveRDS(env_main_occupancy, paste0('plots/int_data_plots/env/sp_env_occ_neonic_7_',region,'_occupancy.rds'))
  
  
  # get occupancy for every species with nenonic set to zero.
  
  main_occupancy_neonic <- lapply(1:nrow(sp_yr), FUN = function(x) get.y.val.all.neonic(sims.mat = sims.mat, ss = sp_yr$sp[x], my.data = my.data,
                                                                          yr = sp_yr$yr[x], species_directory = species_directory, region = NA))

  env_main_occupancy_neonic <- rbindlist(main_occupancy_neonic)

  saveRDS(env_main_occupancy_neonic, paste0('plots/int_data_plots/env/sp_env_occ_neonic_0_',region,'_occupancy.rds'))

  
}


region_v <- c("West", "Center","SouthEast", "NorthEast")

lapply(region_v, create_data_for_plots)


#### calculate the decrease in area due to neonic use ###

region_v <- c("West","Center","NorthEast", "SouthEast")

for(reg in region_v){
  
  ## load data
  
  my.data <- readRDS(paste0("clean_data/data_prepared/my_data_env_genus_counties_1995_2015_ALL_", reg,".rds"))
  
  species_directory <- data.frame(finalName = my.data$sp) %>% 
    mutate(ss = 1:n()) %>% 
    left_join(my.data[[2]])
  
  env_main_occupancy <- readRDS(paste0('plots/int_data_plots/env/sp_env_occ_neonic_7_',reg,'_occupancy.rds'))
  
  env_main_occupancy_neonic <- readRDS(paste0('plots/int_data_plots/env/sp_env_occ_neonic_0_',reg,'_occupancy.rds'))

  area <- readRDS("clean_data/sites/area_counties.RDS") %>% 
    mutate(site = paste0("s_", state_county)) %>% 
    mutate(area_km2 = area_m_2 * 1e-6)
  
  ## Calculate range area for each species 
  
  area_occ_orig <- list()
  area_no_neonic <- list()
  area_dif <- list()
  area_percent <- list()
  
  for(sp in 1:max(species_directory$ss)) {
    
    ## filter the species
    sp_name <- species_directory %>% 
      filter(ss == sp) %>% 
      select(finalName) %>% unlist()
    
    ## get ranges for that species in that region 
    
    sites_sp <- my.data[[3]][[sp_name]]
    
    sites_in_region <- rownames(my.data[[1]]$tmax)
    
    species_sites_in_region <- sites_sp[sites_sp %in% sites_in_region]
    
    ## calculate the area of species sites in region ##
    
    total_area_range_species <- area %>% 
      filter(site %in% species_sites_in_region) %>% 
      select(area_km2) %>% 
      colSums()
    
    calc_area <- function(x) (x* total_area_range_species)
    
    env_main_occupancy_oc <- env_main_occupancy %>% 
      filter(ss == sp) %>% 
      select(mean, `X2.5.`, `X97.5.`)
    
    env_main_occupancy_final <- env_main_occupancy_oc %>% 
      mutate_all(calc_area) %>% 
      bind_cols(env_main_occupancy_oc)
    
    env_main_occupancy_neonic_oc <- env_main_occupancy_neonic %>% 
      filter(ss == sp) %>% 
      select(mean, `X2.5.`, `X97.5.`)
    
    env_main_occupancy_final_neonic <- env_main_occupancy_neonic_oc %>% 
      mutate_all(calc_area) %>% 
      bind_cols(env_main_occupancy_neonic_oc)
    
    Area_percent <- (env_main_occupancy_final - env_main_occupancy_final_neonic)/env_main_occupancy_final
    
    Area_dif <-  env_main_occupancy_final - env_main_occupancy_final_neonic
    
    area_occ_orig[[sp]] <- mutate(env_main_occupancy_final, species = sp)
  
    area_no_neonic[[sp]] <- mutate(env_main_occupancy_final_neonic, species = sp)
    
    area_dif[[sp]] <- mutate(Area_dif, species = sp)
    
    area_percent[[sp]] <- mutate(Area_percent, species = sp)
    
  }
  
  saveRDS(list(area_occ_orig, area_no_neonic, area_dif, area_percent), 
          paste0("plots/area_neonics/int_data/", reg, ".rds"))
  
  
}

### make regional plots for area change 

region_v <- c("West","Center","NorthEast", "SouthEast")

for(reg in region_v){

  all_outputs_neonic <- readRDS(paste0("plots/area_neonics/int_data/", reg, ".rds"))
  
  reg_neonic_plot <- all_outputs_neonic[[4]] %>% 
    rbindlist() %>%
    arrange(`mean...1`) %>% 
    mutate(rank = 1:n()) %>% 
    ggplot() + geom_point(aes(x = rank, y = `mean...1`)) +
    geom_errorbar(aes(x = rank, ymin = `X2.5....2`, ymax = `X97.5....3`)) +
    geom_hline(aes(yintercept = 0), colour = 'red') +
    ylab("") + xlab("")  +
    theme_cowplot() +
    theme(axis.line.x = element_blank(), 
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()) +
    scale_y_continuous(limits = c(-1.7, 0.5))
  
  ggsave(reg_neonic_plot, filename = paste0("plots/area_neonics/", reg, ".jpeg"))
     
}


area_dif %>% 
  rbindlist() 

area_percent %>% 
  rbindlist() %>%
  arrange(`mean...1`) %>% 
  mutate(rank = 1:n()) %>% 
  ggplot() + geom_point(aes(x = rank, y = `mean...1`)) +
  geom_errorbar(aes(x = rank, ymin = `X2.5....2`, ymax = `X97.5....3`)) +
  geom_hline(aes(yintercept = 0), colour = 'red') +
  ylab("(Area in 2015 - Area with 0 neonicotioids) / Area in 2015")


area_dif %>% 
  rbindlist() %>%
  arrange(`mean...1`) %>% 
  mutate(rank = 1:n()) %>% 
  ggplot() + geom_point(aes(x = rank, y = `mean...1`)) +
  geom_errorbar(aes(x = rank, ymin = `X2.5....2`, ymax = `X97.5....3`)) +
  geom_hline(aes(yintercept = 0), colour = 'red') +
  ylab("Area in in 2015 - Area with 0 neonicotioids")



area_dif %>% 
  rbindlist() %>%
  ggplot() + geom_histogram(aes(x = `mean...1`), bins = 100)  +
  geom_vline(aes(xintercept = 0), colour = 'red') +
  xlab("Area in in 2015 - Area with 0 neonicotioids")


area_percent  %>% 
  rbindlist() %>%
  ggplot() + geom_histogram(aes(x = `mean...1`), bins = 100)  +
  geom_vline(aes(xintercept = 0), colour = 'red') +
  xlab("(Area in 2015 - Area with 0 neonicotioids) / Area in 2015")
