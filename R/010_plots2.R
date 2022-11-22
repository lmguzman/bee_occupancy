library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot)

### make plots ##

col_selector <- function(env_var){
  list('temp'='tt1','prec' = "pp", 'neonic' = 'nn', 'agriculture' = 'ag')[[env_var]]
}

xvar_selector <- function(env_var){
  list('temp' = 'Temperature', 'prec' = "Precipitation", 'neonic' = 'Neonicotinoid Use', 'agriculture' = 'Agrilcultural cover')[[env_var]]
}


make_plots <- function(env_var, region){
  
    #species
    species_occ <- readRDS(paste0('plots/int_data_plots_good_results_wrongT/species_',region,"_" ,env_var,'_occupancy.rds')) 
    
    ## genus 
    genus_occ <- readRDS(paste0('plots/int_data_plots_good_results_wrongT/genus_',region,"_" ,env_var,'_occupancy.rds')) %>% 
      rename(genus = gg)
    
    ## main
    main_occ<- readRDS(paste0('plots/int_data_plots_good_results_wrongT/main_',region,"_" ,env_var,'_occupancy.rds')) 
  
  
  ## plot 
  
  ## make array of chains for a genus and then calculate the average across species for each genus
  ## add vertical bar at the peak of each curve
  
  species_genus_plot <- species_occ %>% 
    filter(!is.na(mean)) %>% 
    ggplot() +
    geom_line(colour = 'grey', aes_string(x = col_selector(env_var), y = "mean", group = "finalName")) +
    facet_wrap(~genus) +
    theme_cowplot() +
    geom_line(data = genus_occ, aes_string(x = col_selector(env_var), y = "mean"), colour = 'black') +
    geom_line(data = genus_occ, aes_string(x = col_selector(env_var), y = "X2.5."), colour = 'black', linetype = 'dashed') +
    geom_line(data = genus_occ, aes_string(x = col_selector(env_var), y = "X97.5."), colour = 'black', linetype = 'dashed') +
    xlab(xvar_selector(env_var)) + ylab("Occupancy") +
    ggtitle(region)
  
  ggsave(species_genus_plot, filename = paste0("plots/species_genus_plot_", region, "_", env_var, ".jpeg"))
  
  main_plot <- ggplot() +
    geom_line(data = main_occ, aes_string(x = col_selector(env_var), y = "mean"), colour = 'black') +
    geom_ribbon(data = main_occ, aes_string(x = col_selector(env_var), ymin = "X2.5.", ymax = "X97.5."), alpha = 0.2) +
    theme_cowplot() +
    xlab(xvar_selector(env_var)) + ylab("Occupancy")
  
  ggsave(main_plot, filename = paste0("plots/main_plot_plot_", region, "_", env_var, ".jpeg"))
  
  
}

all_combs <- expand.grid(ev_col = c('temp', 'prec', 'neonic', 'agriculture'), region_col = c("Center","NorthEast", "SouthEast", "West"))

lapply(1:nrow(all_combs), FUN = function(x) make_plots(all_combs$ev_col[x], all_combs$region_col[x]))























region <- "Center"
env_var <- "temp"
if(env_var == 'temp'){
  
  #species
  species_occ <- readRDS(paste0('plots/int_data_plots/species_',region,"_" ,env_var,'_occupancy.rds')) 
    #left_join(temp_scaled, by = c("tt1" = "scaled_p"))
  
  ## genus 
  genus_occ <- readRDS(paste0('plots/int_data_plots/genus_',region,"_" ,env_var,'_occupancy.rds')) %>% 
    #left_join(temp_scaled, by = c("tt1" = "scaled_p")) %>% 
    rename(genus = gg)
  
  ## main
  #temp_df <- species_occ %>% select(tt1, tt2, max_t_year) %>% unique() %>% filter(!is.na(tt1))
  main_occ<- readRDS(paste0('plots/int_data_plots/main_',region,"_" ,env_var,'_occupancy.rds'))  
    #bind_cols(temp_df)
  
  
  species_genus_plot <- species_occ %>% 
    filter(!is.na(mean)) %>% 
    ggplot() +
    geom_line(colour = 'grey', aes_string(x = "tt1", y = "mean", group = "finalName")) +
    facet_wrap(~genus) +
    theme_cowplot() +
    geom_line(data = genus_occ, aes_string(x = "tt1", y = "mean"), colour = 'black') +
    geom_line(data = genus_occ, aes_string(x = "tt1", y = "X2.5."), colour = 'black', linetype = 'dashed') +
    geom_line(data = genus_occ, aes_string(x = "tt1", y = "X97.5."), colour = 'black', linetype = 'dashed') +
    xlab(xvar_selector(env_var)) + ylab("Occupancy") +
    ggtitle(region)
  
  ggsave(species_genus_plot, filename = paste0("plots/species_genus_plot_", region, "_", env_var, ".jpeg"))
  
  main_plot <- ggplot() +
    geom_line(data = main_occ, aes_string(x = "tt1", y = "mean"), colour = 'black') +
    geom_ribbon(data = main_occ, aes_string(x = "tt1", ymin = "X2.5.", ymax = "X97.5."), alpha = 0.2) +
    theme_cowplot() +
    xlab(xvar_selector(env_var)) + ylab("Occupancy")
  
  ggsave(main_plot, filename = paste0("plots/main_plot_plot_", region, "_", env_var, ".jpeg"))
  
  
  
  
}else{
  





## tmax plot

## species occ
species_tmax_occuapncy_clean <- species_tmax_occupancy_m18[,.(ss, tt1, mean)] %>% 
  left_join(species_directory, by = c("ss" = "sp_n")) %>% 
  left_join(temp_filtered, by = c('tt1' = 'X1'))

## genus occ

genus_tmax_occuapncy_clean <- genus_tmax_occupancy_m18[,.(gg, tt1, mean, X2.5., X97.5.)] %>% 
  left_join(temp_filtered, by = c('tt1' = 'X1')) %>% 
  rename(genus = gg)

main_tmax_occupancy <- get.y.val.main.all("temp")

## calculate main occupancy 

main_tmax_occuapncy_clean <- bind_cols(main_tmax_occupancy, temp_filtered)

## plot 

## make array of chains for a genus and then calculate the average across species for each genus
## add vertical bar at the peak of each curve

species_tmax_occuapncy_clean %>% 
  ggplot() +
  geom_line(colour = 'grey', aes(x = max_t_year, y = mean, group = sp_name)) +
  facet_wrap(~genus) +
  theme_cowplot() +
  # geom_line(data = main_tmax_occuapncy_clean, aes(x = max_t_year, y = mean), colour = 'black') +
  # geom_line(data = main_tmax_occuapncy_clean, aes(x = max_t_year, y = `X2.5.`), colour = 'black', linetype = 'dashed') +
  # geom_line(data = main_tmax_occuapncy_clean, aes(x = max_t_year, y = `X97.5.`), colour = 'black', linetype = 'dashed') +
  geom_line(data = genus_tmax_occuapncy_clean, aes(x = max_t_year, y = mean), colour = 'black') +
  geom_line(data = genus_tmax_occuapncy_clean, aes(x = max_t_year, y = `X2.5.`), colour = 'black', linetype = 'dashed') +
  geom_line(data = genus_tmax_occuapncy_clean, aes(x = max_t_year, y = `X97.5.`), colour = 'black', linetype = 'dashed') +
  xlab("Maximum Temperature") + ylab("Occupancy")




## precipitation plot


## species 
species_prec_occuapncy_clean <- species_prec_occupancy_m18[,.(ss, pp, mean)] %>% 
  left_join(species_directory, by = c("ss" = "sp_n")) 

## genus

genus_prec_occuapncy_clean <- genus_prec_occupancy_m18[,.(gg, pp, mean, X2.5., X97.5.)] %>% 
  rename(genus = gg)

## calculate main occupancy 

main_prec_occupancy <- get.y.val.main.all("prec")

main_prec_occuapncy_clean <- bind_cols(main_prec_occupancy, pp =seq(from=min(my.data[[1]]$prec),  
                                                                    to=  max(my.data[[1]]$prec),
                                                                    length.out=1000))

## plot 

## make this as a linear function instead of quadratic 
quantile(sims.mat[,'mu.psi.prec'], c(0.025, 0.5, 0.975))

species_prec_occuapncy_clean %>% 
  ggplot() +
  geom_line(colour = 'grey', aes(x = pp, y = mean, group = sp_name)) +
  facet_wrap(~genus) +
  theme_cowplot() +
  xlab("Total Precipitation") + ylab("Occupancy") +
  # geom_line(data = main_prec_occuapncy_clean, aes(x = pp, y = mean), colour = 'black') +
  # geom_line(data = main_prec_occuapncy_clean, aes(x = pp, y = `X2.5.`), colour = 'black', linetype = 'dashed') +
  # geom_line(data = main_prec_occuapncy_clean, aes(x = pp, y = `X97.5.`), colour = 'black', linetype = 'dashed') 
  geom_line(data = genus_prec_occuapncy_clean, aes(x = pp, y = mean), colour = 'black') +
  geom_line(data = genus_prec_occuapncy_clean, aes(x = pp, y = `X2.5.`), colour = 'black', linetype = 'dashed') +
  geom_line(data = genus_prec_occuapncy_clean, aes(x = pp, y = `X97.5.`), colour = 'black', linetype = 'dashed') 




##neonic plot

# species
species_neonic_occuapncy_clean <- species_neonic_occupancy_m18[,.(ss, nn, mean)] %>% 
  left_join(species_directory, by = c("ss" = "sp_n")) 

# genus

## genus

genus_neonic_occuapncy_clean <- genus_neonic_occupancy_m18[,.(gg, nn, mean, X2.5., X97.5.)] %>% 
  rename(genus = gg)

## calculate main occupancy 

main_neonic_occupancy <- get.y.val.main.all("neonic")

main_neonic_occuapncy_clean <- bind_cols(main_neonic_occupancy, nn =seq(from=min(my.data[[1]]$pesticide1),  
                                                                        to=  max(my.data[[1]]$pesticide1),
                                                                        length.out=1000))

## plot 

species_neonic_occuapncy_clean %>% 
  ggplot() +
  geom_line(colour = 'grey', aes(x = nn, y = mean, group = sp_name)) +
  facet_wrap(~genus) +
  theme_cowplot() +
  geom_line(data = genus_neonic_occuapncy_clean, aes(x = nn, y = mean), colour = 'black') +
  geom_line(data = genus_neonic_occuapncy_clean, aes(x = nn, y = `X2.5.`), colour = 'black', linetype = 'dashed') +
  geom_line(data = genus_neonic_occuapncy_clean, aes(x = nn, y = `X97.5.`), colour = 'black', linetype = 'dashed') +
  xlab("Neonicotinoid") + ylab("Occupancy")




## pest_gen plot

# species
species_pest_gen_occuapncy_clean <- species_pest_gen_occupancy_m18[,.(ss, gt, mean)] %>% 
  left_join(species_directory, by = c("ss" = "sp_n")) 

# genus

## genus

genus_pest_gen_occuapncy_clean <- genus_pest_gen_occupancy_m18[,.(gg, gt, mean, X2.5., X97.5.)] %>% 
  rename(genus = gg)

## calculate main occupancy 

main_pest_gen_occupancy <- get.y.val.main.all("pest_gen")

main_pest_gen_occuapncy_clean <- bind_cols(main_pest_gen_occupancy, gt =seq(from=min(my.data[[1]]$pesticide2),  
                                                                            to=  max(my.data[[1]]$pesticide2),
                                                                            length.out=1000))

## plot 

species_pest_gen_occuapncy_clean %>% 
  ggplot() +
  geom_line(colour = 'grey', aes(x = gt, y = mean, group = sp_name)) +
  facet_wrap(~genus) +
  theme_cowplot() +
  geom_line(data = genus_pest_gen_occuapncy_clean, aes(x = gt, y = mean), colour = 'black') +
  geom_line(data = genus_pest_gen_occuapncy_clean, aes(x = gt, y = `X2.5.`), colour = 'black', linetype = 'dashed') +
  geom_line(data = genus_pest_gen_occuapncy_clean, aes(x = gt, y = `X97.5.`), colour = 'black', linetype = 'dashed') +
  xlab("General Pesticide") + ylab("Occupancy")




## pyr plot

# species
species_pyr_occuapncy_clean <- species_pyr_occupancy_m18[,.(ss, py, mean)] %>% 
  left_join(species_directory, by = c("ss" = "sp_n")) 

# genus

## genus

genus_pyr_occuapncy_clean <- genus_pyr_occupancy_m18[,.(gg, py, mean, X2.5., X97.5.)] %>% 
  rename(genus = gg)

## calculate main occupancy 

main_pyr_occupancy <- get.y.val.main.all("pyr")

main_pyr_occuapncy_clean <- bind_cols(main_pyr_occupancy, gt =seq(from=min(my.data[[1]]$pesticide3),  
                                                                  to=  max(my.data[[1]]$pesticide3),
                                                                  length.out=1000))

## plot 

species_pyr_occuapncy_clean %>% 
  ggplot() +
  geom_line(colour = 'grey', aes(x = py, y = mean, group = sp_name)) +
  facet_wrap(~genus) +
  theme_cowplot() +
  geom_line(data = genus_pyr_occuapncy_clean, aes(x = py, y = mean), colour = 'black') +
  geom_line(data = genus_pyr_occuapncy_clean, aes(x = py, y = `X2.5.`), colour = 'black', linetype = 'dashed') +
  geom_line(data = genus_pyr_occuapncy_clean, aes(x = py, y = `X97.5.`), colour = 'black', linetype = 'dashed') +
  xlab("Pyrethroid") + ylab("Occupancy")





## agriculture

# species
species_agriculture_occuapncy_clean <- species_agriculture_occupancy_m18[,.(ss, ag, mean)] %>% 
  left_join(species_directory, by = c("ss" = "sp_n")) 

# genus

## genus

genus_agriculture_occuapncy_clean <- genus_agriculture_occupancy_m18[,.(gg, ag, mean, X2.5., X97.5.)] %>% 
  rename(genus = gg)

## calculate main occupancy 

main_agriculture_occupancy <- get.y.val.main.all("agriculture")

main_agriculture_occuapncy_clean <- bind_cols(main_agriculture_occupancy, gt =seq(from=min(my.data[[1]]$agriculture),  
                                                                                  to=  max(my.data[[1]]$agriculture),
                                                                                  length.out=1000))

## plot 

species_agriculture_occuapncy_clean %>% 
  ggplot() +
  geom_line(colour = 'grey', aes(x = ag, y = mean, group = sp_name)) +
  facet_wrap(~genus) +
  theme_cowplot() +
  geom_line(data = genus_agriculture_occuapncy_clean, aes(x = ag, y = mean), colour = 'black') +
  geom_line(data = genus_agriculture_occuapncy_clean, aes(x = ag, y = `X2.5.`), colour = 'black', linetype = 'dashed') +
  geom_line(data = genus_agriculture_occuapncy_clean, aes(x = ag, y = `X97.5.`), colour = 'black', linetype = 'dashed') +
  xlab("Agriculture") + ylab("Occupancy")



