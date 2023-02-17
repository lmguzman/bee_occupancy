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
  
  species_directory <- data.frame(finalName = my.data$sp) %>% 
    mutate(sp_n = 1:n()) %>% 
    left_join(my.data[[2]])
  
  # Get occupancy for every species only in year 7 (after neonics have been applied)
  
  
  ### get chains instead of summaries --calculate the difference in occupancy with the chain and then condifernece intervals of the difference 
  ### to get summaries of the region -- calculate the average between species in a region, then calculate mean across chain -- credible interval for something
  ### also do this for the mu.psi.everything -- second pass for the regional with mean temperature, mean prec
  # 
  # sp_yr <- expand.grid(yr = 7, sp = 1:nsp)
  # 
  # main_occupancy <- lapply(1:nrow(sp_yr), FUN = function(x) get.y.val.all(sims.mat = sims.mat, ss = sp_yr$sp[x], my.data = my.data,
  #                                                                         yr = sp_yr$yr[x], species_directory = species_directory, region = NA))
  # 
  # env_main_occupancy <- rbindlist(main_occupancy)
  # 
  # saveRDS(env_main_occupancy, paste0('plots/int_data_plots/env/sp_env_occ_neonic_7_',region,'_occupancy.rds'))
  # 
  # 
  # # get occupancy for every species with nenonic set to zero.
  # 
  # main_occupancy_neonic <- lapply(1:nrow(sp_yr), FUN = function(x) get.y.val.all.neonic(sims.mat = sims.mat, ss = sp_yr$sp[x], my.data = my.data,
  #                                                                         yr = sp_yr$yr[x], species_directory = species_directory, region = NA))
  # 
  # env_main_occupancy_neonic <- rbindlist(main_occupancy_neonic)
  # 
  # saveRDS(env_main_occupancy_neonic, paste0('plots/int_data_plots/env/sp_env_occ_neonic_0_',region,'_occupancy.rds'))

  ## bottom vs top 25 % sites 
  
  sp_yr <- expand.grid(yr = c(1,7), sp = 1:nsp)
  
  
  main_occupancy_neonic <- lapply(1:nrow(sp_yr), FUN = function(x) get.y.val.all.top.bottom(sims.mat = sims.mat, ss = sp_yr$sp[x], my.data = my.data,
                                                                                        yr = sp_yr$yr[x], species_directory = species_directory, region = NA))
  
  env_main_occupancy_neonic <- rbindlist(main_occupancy_neonic)
  
  saveRDS(env_main_occupancy_neonic, paste0('plots/int_data_plots/env/sp_env_occ_top_bottom_neonic_',region,'_occupancy.rds'))
  
  
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
  
  occ_area <- list()

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
    
    env_main_occupancy_oc <- env_main_occupancy %>% 
      filter(ss == sp) %>% 
      select(occ_yr_7 = chains) 
    
    env_main_occupancy_neonic_oc <- env_main_occupancy_neonic %>% 
      filter(ss == sp) %>% 
      select(occ_yr_7_minus_neonic = chains)
    
    occ_area[[sp]] <- bind_cols(env_main_occupancy_oc, env_main_occupancy_neonic_oc) %>% 
      mutate(area_yr_7 = occ_yr_7* total_area_range_species) %>% 
      mutate(area_yr_7_minus_neonic = occ_yr_7_minus_neonic* total_area_range_species) %>% 
      mutate(diff_occu = occ_yr_7- occ_yr_7_minus_neonic) %>% 
      mutate(area_percent = (area_yr_7 - area_yr_7_minus_neonic)/area_yr_7) %>% 
      mutate(species = sp_name)
    
  }
  
  saveRDS(occ_area, 
          paste0("plots/area_neonics/int_data/", reg, ".rds"))
  
  
}



#### calcualte difference in occupancy between the site with most and least neonic ###

region_v <- c("West","Center","NorthEast", "SouthEast")

for(reg in region_v){
  
  ## load data
  
  my.data <- readRDS(paste0("clean_data/data_prepared/my_data_env_genus_counties_1995_2015_ALL_", reg,".rds"))
  
  species_directory <- data.frame(finalName = my.data$sp) %>% 
    mutate(ss = 1:n()) %>% 
    left_join(my.data[[2]])
  
  env_main_occupancy_neonic <- readRDS(paste0('plots/int_data_plots/env/sp_env_occ_top_bottom_neonic_',reg,'_occupancy.rds'))
  
  ## Calculate range area for each species 
  
  occ_area <- list()
  
  for(sp in 1:max(species_directory$ss)) {
    
    ## filter the species
    sp_name <- species_directory %>% 
      filter(ss == sp) %>% 
      select(finalName) %>% unlist()
    
    occ_area[[sp]] <- env_main_occupancy_neonic %>% 
      filter(ss == sp) %>% 
      select(yr, chains, sites)  %>% 
      mutate(iter = rep(1:600, 2)) %>% 
      pivot_wider(names_from = 'yr', values_from = 'chains') %>% 
      mutate(dif = `7`-`1`) %>% 
      mutate(species = sp_name)
    
  }
  
  saveRDS(occ_area, 
          paste0("plots/area_neonics/int_data/top_bottom", reg, ".rds"))
  
  
}



### change in occupancy 

region_v <- c("West","Center","NorthEast", "SouthEast")

for(reg in region_v){
  
  all_outputs_neonic <- readRDS(paste0("plots/area_neonics/int_data/", reg, ".rds"))
  
  ## difference in occupancy
  reg_neonic_plot <- all_outputs_neonic %>% 
    rbindlist() %>%
    group_by(species) %>% 
    summarise(mean = mean(diff_occu), x2.5 = quantile(diff_occu, 0.025), x97.5 = quantile(diff_occu, 0.975)) %>% 
    arrange(mean) %>% 
    mutate(rank = 1:n()) %>% 
    mutate(point_col = ifelse(mean < 0, 'neg', 'pos')) %>% 
    mutate(bar_col = ifelse(x2.5 < 0 & x97.5 > 0, "notsig", point_col)) %>% 
    ggplot() + geom_point(aes(x = rank, y = mean, colour = point_col), alpha = 0.6) +
    geom_errorbar(aes(x = rank, ymin = x2.5, ymax = x97.5, colour = bar_col),alpha = 0.6) +
    geom_hline(aes(yintercept = 0), colour = 'black') +
    scale_color_manual("point_col", values = c( 'red', "grey", 'black')) +
    ylab("") + xlab("")  +
    theme_cowplot() +
    theme(axis.line.x = element_blank(), 
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(), 
          axis.text.y = element_text(size = 30),
          legend.position = 'none') +
    scale_y_continuous(limits = c(-0.6,0.3))
  
  ggsave(reg_neonic_plot, filename = paste0("plots/area_neonics/", reg, "diff_occupancy.pdf"))
  
  ## difference in area percent
  reg_neonic_plot_area <- all_outputs_neonic %>% 
    rbindlist() %>%
    mutate(area_percent_zero = (area_yr_7 - area_yr_7_minus_neonic)/area_yr_7_minus_neonic) %>% 
    group_by(species) %>% 
    summarise(mean = mean(area_percent), x2.5 = quantile(area_percent, 0.025), x97.5 = quantile(area_percent, 0.975)) %>% 
    arrange(mean) %>% 
    mutate(rank = 1:n()) %>% 
    mutate(point_col = ifelse(mean < 0, 'neg', 'pos')) %>% 
    mutate(bar_col = ifelse(x2.5 < 0 & x97.5 > 0, "notsig", point_col)) %>% 
    ggplot() + geom_point(aes(x = rank, y = mean, colour = point_col), alpha = 0.6) +
    geom_errorbar(aes(x = rank, ymin = x2.5, ymax = x97.5, colour = bar_col),alpha = 0.6) +
    geom_hline(aes(yintercept = 0), colour = 'black') +
    scale_color_manual("point_col", values = c( 'red', "grey", 'black')) +
    ylab("") + xlab("")  +
    theme_cowplot() +
    theme(axis.line.x = element_blank(), 
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(), 
          axis.text.y = element_text(size = 30),
          legend.position = 'none') 
    scale_y_continuous(limits = c(-15,10))
  
  ggsave(reg_neonic_plot_area, filename = paste0("plots/area_neonics/", reg, ".pdf"))

  
}

### get regional percentages



#### top vs bottom plot 

region_v <- c("West","Center","NorthEast", "SouthEast")

for(reg in region_v){
  
  all_outputs_neonic_top_bottom <- readRDS(paste0("plots/area_neonics/int_data/top_bottom", reg, ".rds"))
  
  species_summarised <- all_outputs_neonic_top_bottom %>% 
    rbindlist() %>%
    group_by(species, sites) %>% 
    summarise(mean = mean(dif, na.rm = TRUE), x2.5 = quantile(dif, 0.025, na.rm = TRUE), x97.5 = quantile(dif, 0.975, na.rm = TRUE)) %>% 
    ungroup()
  
  species_rank <- species_summarised %>% 
    filter(sites == 'top') %>% 
    arrange(mean) %>% 
    select(species) %>% 
    mutate(rank = 1:n())
  
species_summarised %>% 
  left_join(species_rank) %>% 
  arrange(mean) %>% 
  mutate(rank = 1:n()) %>% 
  ggplot() + geom_point(aes(x = rank, y = mean, colour = sites), alpha = 0.9) +
  geom_errorbar(aes(x = rank, ymin = x2.5, ymax = x97.5, colour = sites),alpha = 0.6) +
  geom_hline(aes(yintercept = 0), colour = 'black') +
  facet_wrap(~sites)
  scale_color_manual("point_col", values = c( 'red', "grey", 'black')) +
  ylab("") + xlab("")  +
  theme_cowplot() +
  theme(axis.line.x = element_blank(), 
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(), 
        axis.text.y = element_text(size = 30),
        legend.position = 'none') +
    ggtitle(reg)
}
ggsave(reg_neonic_plot_area, filename = paste0("plots/area_neonics/", reg, ".pdf"))



region_v <- c("West","Center","NorthEast", "SouthEast")

region_mean_chain_area <- list()
region_mean_chain_diff <- list()
n_sp_low <- list()

for(reg in region_v){
  
  all_outputs_neonic <- readRDS(paste0("plots/area_neonics/int_data/", reg, ".rds"))
  
  nsp <- unique(select(rbindlist(all_outputs_neonic), species)) %>% nrow()
  
  mean_chain_area_percent <- all_outputs_neonic %>% 
    rbindlist() %>% 
    select(area_percent, species) %>% 
    mutate(chain = rep(1:300, nsp)) %>% 
    pivot_wider(names_from = species, values_from = area_percent) %>% 
    select(-chain) %>% 
    rowMeans()
  
  mean_chain_diff <- all_outputs_neonic %>% 
    rbindlist() %>% 
    select(diff_occu, species) %>% 
    mutate(chain = rep(1:300, nsp)) %>% 
    pivot_wider(names_from = species, values_from = diff_occu) %>% 
    select(-chain) %>% 
    rowMeans()
  
  mean_species <- all_outputs_neonic %>% 
    rbindlist() %>% 
    group_by(species) %>% 
    summarise(mean_dif = mean(diff_occu), mean_area = mean(area_percent))
  
  region_mean_chain_area[[reg]] <- round(c(mean(mean_chain_area_percent), quantile(mean_chain_area_percent, c(0.025, 0.975))), 3)
  
  region_mean_chain_diff[[reg]] <- round(c(mean(mean_chain_diff), quantile(mean_chain_diff, c(0.025, 0.975))), 3)
  
  n_sp_low[[reg]] <- c(nrow(mean_species), sum(mean_species$mean_dif < 0), sum(mean_species$mean_area < 0)/nsp)
  
}

region_mean_chain_area

region_mean_chain_diff




#### get regional percentages

region_v <- c("West","Center","NorthEast", "SouthEast")

all_regions <- list()

for(reg in region_v){
  
  all_outputs_neonic <- readRDS(paste0("plots/area_neonics/int_data/", reg, ".rds"))
  
  all_regions[[reg]] <- rbindlist(all_outputs_neonic) %>% 
    mutate(percent_lost = area_percent*100) 
  
}

all_regions %>% 
  map_df(~as.data.frame(.x), .id = 'region') %>% 
  group_by(region) %>% 
  summarise(median(percent_lost))


area_dif %>% 
  rbindlist() 

marea_percent %>% 
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



### make regional plots for area change 

region_v <- c("West","Center","NorthEast", "SouthEast")
colours <- c()

for(reg in region_v){
  
  all_outputs_neonic <- readRDS(paste0("plots/area_neonics/int_data/", reg, ".rds"))
  
  reg_neonic_plot <- all_outputs_neonic[[4]] %>% 
    rbindlist() %>%
    arrange(`mean...1`) %>% 
    mutate(rank = 1:n()) %>% 
    ggplot() + geom_point(aes(x = rank, y = `mean...1`*100), colour = 'blue',fill = "blue", alpha = 0.6) +
    geom_errorbar(aes(x = rank, ymin = `X2.5....2`*100, ymax = `X97.5....3`*100), colour = "blue", alpha = 0.6) +
    geom_hline(aes(yintercept = 0), colour = 'red') +
    ylab("") + xlab("")  +
    theme_cowplot() +
    theme(axis.line.x = element_blank(), 
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(), 
          axis.text.y = element_text(size = 20)) +
    scale_y_continuous(limits = c(-170, 50))
  
  ggsave(reg_neonic_plot, filename = paste0("plots/area_neonics/", reg, ".pdf"))
  
}



