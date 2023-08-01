library(ggplot2)
library(cowplot)
library(tidyr)
library(dplyr)
library(RColorBrewer)
library(stringr)
library(data.table)
library(purrr)
source("R/src/initialize.R")
source("R/src/plot_env_functions.R")


create_data_for_plots <- function(f){

  res.summary <- readRDS(paste0("model_outputs/res.summary_genus_filtered_agriregion_pest_area_county_both_1995_2015_ms_area_honeytime_pestar_canag_15_", f,"_ALLFALSE.rds"))
  
  my.data <- readRDS(paste0("clean_data/data_prepared/my_data_env_genus_filtered_trait_agriregion_both_pest_area_county_1995_2015_",f,"_ALLFALSE.rds"))
  
  
  vars <- rownames(res.summary$psrf$psrf)
  summ <- get.summ(vars, res.summary)
  
  summ.paper <- summ[str_detect(rownames(summ), 'mu.psi'),]
  
  ncols_sum <- dim(summ.paper)[2]
  nrows_sum <- dim(summ.paper)[1]
  
  ### assign correct chains ##
  
  sims.mat <- do.call(rbind, res.summary$mcmc)
  sims.arr <-
    aperm(sapply(res.summary$mcmc, I, simplify='array'), c(1,3,2))
  
  nera <- my.data[[1]]$nyr
  nsp <- my.data[[1]]$nsp
  species_directory <- my.data[[2]] %>% 
    mutate(sp_n = 1:n())
  
    # main trend occupancy
    
    main_occupancy <- get.y.val.main.all(sims.mat = sims.mat, my.data = my.data)
    
    saveRDS(main_occupancy, paste0('plots/int_data_plots/env/main_',f,'_occupancy.rds'))
  
}

fam <- c("Andrenidae", "Apidae", "Halictidae", 
         "Megachilidae", "Colletidae|Melittidae")

lapply(fam, create_data_for_plots)


main_family_list <- list()

for(f in fam){
  
  main_family_list[[f]] <- readRDS(paste0('plots/int_data_plots/env/main_',f,'_occupancy.rds')) %>% 
    mutate(family = f) %>% 
    as.data.table()
  
}

main_trend_pes <- main_region_list %>% 
  rbindlist() %>% 
  ggplot() +
  geom_line(aes(x = ps, y = mean)) +
  geom_ribbon(aes(x = ps, ymin = `X2.5.`, ymax = X97.5.), alpha = 0.3) +
  facet_wrap(~family) +
  theme_cowplot() +
  theme(strip.background =element_blank(), 
        axis.text = element_text(size = 15), 
        strip.text = element_text(size = 20), 
        axis.title = element_text(size = 20)) +
  ylab("Occupancy") +
  xlab("Pesticide Use") +
  scale_y_continuous(limits = c(0,1))


ggsave(main_trend_pes, filename = "plots/trends/main_env_pesticide.pdf")


main_region_list %>% 
  rbindlist() %>% 
  group_by(family) %>% 
  summarise(min(ps), max(ps))


main_pest_df <- main_region_list %>% 
  rbindlist()

main_pest_df %>% 
  filter(ps %in% c(min(main_pest_df$ps), max(main_pest_df$ps))) %>% 
  select(mean, ps, family) %>% 
  pivot_wider(names_from = ps, values_from = mean) %>% 
  rename(no_pest = `-4.17560742453957`, max_pest = `1.77649633931698`) %>% 
  mutate(net_change = no_pest - max_pest, percent_change = (no_pest - max_pest)/no_pest)
  
##### at the genus level for pesticide  #####

create_data_for_plots <- function(f){
  
  res.summary <- readRDS(paste0("model_outputs/res.summary_genus_filtered_agriregion_pest_area_county_both_1995_2015_ms_area_honeytime_pestar_canag_15_", f,"_ALLFALSE.rds"))
  
  my.data <- readRDS(paste0("clean_data/data_prepared/my_data_env_genus_filtered_trait_agriregion_both_pest_area_county_1995_2015_",f,"_ALLFALSE.rds"))
  
  vars <- rownames(res.summary$psrf$psrf)
  summ <- get.summ(vars, res.summary)
  
  summ.paper <- summ[str_detect(rownames(summ), 'mu.psi'),]
  
  ncols_sum <- dim(summ.paper)[2]
  nrows_sum <- dim(summ.paper)[1]
  
  ### assign correct chains ##
  
  sims.mat <- do.call(rbind, res.summary$mcmc)
  sims.arr <-
    aperm(sapply(res.summary$mcmc, I, simplify='array'), c(1,3,2))
  
  nera <- my.data[[1]]$nyr
  nsp <- my.data[[1]]$nsp
  species_directory <- my.data[[2]] %>% 
    mutate(sp_n = 1:n())
  
  # main trend occupancy
  
  main_occupancy <- get.y.val.genus.all(sims.mat = sims.mat, my.data = my.data, species_directory = species_directory)
  
  saveRDS(main_occupancy, paste0('plots/int_data_plots/env/genus_',f,'_occupancy.rds'))
  
}


fam <- c("Andrenidae", "Apidae", "Halictidae", 
         "Megachilidae", "Colletidae|Melittidae")

lapply(fam, create_data_for_plots)


genus_family_list <- list()

for(f in fam){
  
  genus_family_list[[f]] <- readRDS(paste0('plots/int_data_plots/env/genus_',f,'_occupancy.rds')) %>% 
    mutate(family = f) %>% 
    as.data.table()
  
}

genus_trend_pes <- genus_family_list %>% 
  rbindlist() %>% 
  ggplot() +
  facet_wrap(~family) +
  geom_line(aes(x = ps, y = mean, group = gg)) +
  geom_ribbon(aes(x = ps, ymin = `X2.5.`, ymax = X97.5., group = gg), alpha = 0.1) +
  theme_cowplot() +
  theme(strip.background =element_blank(), 
        axis.text = element_text(size = 15), 
        strip.text = element_text(size = 20), 
        axis.title = element_text(size = 20)) +
  ylab("Occupancy") +
  xlab("Pesticide Use") +
  scale_y_continuous(limits = c(0,1))


ggsave(genus_trend_pes, filename = "plots/trends/genus_env_pesticide.pdf")

####

genus_family_list %>% 
  rbindlist() %>% 
  group_by(family, gg) %>% 
  summarise(min(ps), max(ps))

genus_family_pest_df <- genus_family_list %>% 
  rbindlist()

genus_family_pest_df %>% 
  filter(ps %in% c(min(main_pest_df$ps), max(main_pest_df$ps))) %>% 
  select(mean, ps, family, gg) %>% 
  pivot_wider(names_from = ps, values_from = mean) %>% 
  rename(no_pest = `-4.17560742453957`, max_pest = `1.77649633931698`) %>% 
  mutate(net_change = no_pest - max_pest, percent_change = 100*(no_pest - max_pest)/no_pest) %>% 
  arrange(percent_change) %>% View()





##### at the genus level for animal pollinated agriculture  #####

create_data_for_plots <- function(f){
  
  res.summary <- readRDS(paste0("model_outputs/res.summary_genus_filtered_agriregion_pest_area_county_both_1995_2015_ms_area_climate_canag_17_", f,"_ALLFALSE.rds"))
  
  my.data <- readRDS(paste0("clean_data/data_prepared/my_data_env_genus_filtered_trait_agriregion_both_pest_area_county_1995_2015_",f,"_ALLFALSE.rds"))
  
  vars <- rownames(res.summary$psrf$psrf)
  summ <- get.summ(vars, res.summary)
  
  summ.paper <- summ[str_detect(rownames(summ), 'mu.psi'),]
  
  ncols_sum <- dim(summ.paper)[2]
  nrows_sum <- dim(summ.paper)[1]
  
  ### assign correct chains ##
  
  sims.mat <- do.call(rbind, res.summary$mcmc)
  sims.arr <-
    aperm(sapply(res.summary$mcmc, I, simplify='array'), c(1,3,2))
  
  nera <- my.data[[1]]$nyr
  nsp <- my.data[[1]]$nsp
  species_directory <- my.data[[2]] %>% 
    mutate(sp_n = 1:n())
  
  # main trend occupancy
  
  main_occupancy <- get.y.val.genus.all.canag(sims.mat = sims.mat, my.data = my.data, species_directory = species_directory)
  
  saveRDS(main_occupancy, paste0('plots/int_data_plots/env/genus_',f,'_occupancy_canag.rds'))
  
}


fam <- c("Andrenidae", "Apidae", "Halictidae", 
         "Megachilidae", "Colletidae|Melittidae")

lapply(fam, create_data_for_plots)


genus_family_list <- list()

for(f in fam){
  
  genus_family_list[[f]] <- readRDS(paste0('plots/int_data_plots/env/genus_',f,'_occupancy_canag.rds')) %>% 
    mutate(family = f) %>% 
    as.data.table()
  
}

genus_trend_canag <- genus_family_list %>% 
  rbindlist() %>% 
  ggplot() +
  facet_wrap(~family) +
  geom_line(aes(x = ag, y = mean, group = gg)) +
  geom_ribbon(aes(x = ag, ymin = `X2.5.`, ymax = X97.5., group = gg), alpha = 0.1) +
  theme_cowplot() +
  theme(strip.background =element_blank(), 
        axis.text = element_text(size = 15), 
        strip.text = element_text(size = 20), 
        axis.title = element_text(size = 20)) +
  ylab("Occupancy") +
  xlab("Animal Pollinated Agriculture") +
  scale_y_continuous(limits = c(0,1))


ggsave(genus_trend_canag, filename = "plots/trends/genus_env_canag.pdf")

####

min_max_family <- genus_family_list %>% 
  rbindlist() %>% 
  group_by(family) %>% 
  summarise(min_ag = min(ag), max_ag = max(ag)) %>% 
  pivot_longer(names_to = 'min_max', values_to = 'ag', -family)
  

genus_family_canag_df <- genus_family_list %>% 
  rbindlist()

min_max_family %>% 
  left_join(genus_family_canag_df) %>% 
  select(family, gg, min_max, mean) %>% 
  pivot_wider(names_from = min_max, values_from = mean) %>% 
  mutate(net_change = min_ag - max_ag, percent_change = 100*(min_ag - max_ag)/min_ag) %>% 
  arrange(percent_change) %>% View()
