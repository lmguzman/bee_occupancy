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


create_data_for_plots <- function(region){
  
  family <- "ALL"
  year_range <- c(1995, 2015)
  
  # env era
  #model <- "ms_env_area_2"
  model <- "ms_env_area_2_uncons"
  
  res <- readRDS(paste0("model_outputs/env/res_counties_", paste0(year_range, collapse = "_"), "_",model,"_", family, "_", region, ".rds"))
  
  res.summary <- readRDS(paste0("model_outputs/env/res.summary_counties_", paste0(year_range, collapse = "_"), "_",model,"_", family, "_", region, ".rds"))
  
  my.data <- readRDS(paste0("clean_data/data_prepared/my_data_env_genus_counties_", paste0(year_range, collapse = "_"),  "_", family, "_", region, ".rds"))
  
  
  vars <- rownames(res.summary$psrf$psrf)
  summ <- get.summ(vars, res.summary)
  
  summ.paper <- summ[str_detect(rownames(summ), 'mu.psi'),]
  
  ncols_sum <- dim(summ.paper)[2]
  nrows_sum <- dim(summ.paper)[1]
  
  write(region,file="summary_outputs/main_env.txt",append=TRUE)
  write(c("            ",colnames(summ.paper)),,file="summary_outputs/main_env.txt", append=TRUE, ncolumns = (ncols_sum + 1))
  
  for(i in 1:nrows_sum){
    write(paste(rownames(summ.paper)[i], paste(summ.paper[i,], collapse = " ")),file="summary_outputs/main_env.txt",append=TRUE)
  }
  
  ### assign correct chains ##
  
  sims.mat <- do.call(rbind, res$mcmc)
  sims.arr <-
    aperm(sapply(res$mcmc, I, simplify='array'), c(1,3,2))
  
  nera <- my.data[[1]]$nyr
  nsp <- my.data[[1]]$nsp
  species_directory <- my.data[[2]] %>% 
    mutate(sp_n = 1:n())
  
  
  env_list <- c('temp', 'prec', 'neonic', 'agriculture')
  

  for(env in env_list){
    print(env)
    
    # main trend occupancy
    
    main_occupancy <- get.y.val.main.all(sims.mat = sims.mat, env, my.data = my.data)
    
    saveRDS(main_occupancy, paste0('plots/int_data_plots/env/main_',region,"_", env,'_occupancy.rds'))
    
  }
  
}

region_v <- c("West", "Center","SouthEast", "NorthEast")

lapply(region_v, create_data_for_plots)


combs_env_rg <- expand.grid(region_v = c("West", "Center","SouthEast", "NorthEast"), env_list = c('temp', 'prec', 'neonic', 'agriculture'))

main_region_list <- list()

for(r in 1:nrow(combs_env_rg)){
  
  main_region_list[[r]] <- readRDS(paste0('plots/int_data_plots/env/main_',combs_env_rg$region_v[r],"_", combs_env_rg$env_list[r],'_occupancy.rds')) %>% 
    mutate(region = combs_env_rg$region_v[r], env = combs_env_rg$env_list[r]) %>% 
    as.data.table()
  
}

region_nice_name <- data.frame(region = region_v, region_nice = c("West", "Center", "South East", "North East"))

main_trend_prec <- main_region_list %>% 
  rbindlist() %>% 
  left_join(region_nice_name) %>% 
  filter(env == "prec") %>% 
  ggplot() +
  geom_line(aes(x = pp, y = mean)) +
  geom_ribbon(aes(x = pp, ymin = `X2.5.`, ymax = X97.5.), alpha = 0.3) +
  facet_wrap(~region_nice) +
  theme_cowplot() +
  theme(strip.background =element_blank(), 
        axis.text = element_text(size = 15), 
        strip.text = element_text(size = 20), 
        axis.title = element_text(size = 20)) +
  ylab("Occupancy") +
  xlab("Precipitation") 


ggsave(main_trend_prec, filename = "plots/trends/main_env_prec.jpeg")



main_trend_temp <- main_region_list %>% 
  rbindlist() %>% 
  left_join(region_nice_name) %>% 
  filter(env == "temp") %>% 
  ggplot() +
  geom_line(aes(x = tt1, y = mean)) +
  geom_ribbon(aes(x = tt1, ymin = `X2.5.`, ymax = X97.5.), alpha = 0.3) +
  facet_wrap(~region_nice) +
  theme_cowplot() +
  theme(strip.background =element_blank(), 
        axis.text = element_text(size = 15), 
        strip.text = element_text(size = 20), 
        axis.title = element_text(size = 20)) +
  ylab("Occupancy") +
  xlab("Temperature") 


ggsave(main_trend_temp, filename = "plots/trends/main_env_temp.jpeg")



main_trend_agri <- main_region_list %>% 
  rbindlist() %>% 
  left_join(region_nice_name) %>% 
  filter(env == "agriculture") %>% 
  ggplot() +
  geom_line(aes(x = ag, y = mean)) +
  geom_ribbon(aes(x = ag, ymin = `X2.5.`, ymax = X97.5.), alpha = 0.3) +
  facet_wrap(~region_nice) +
  theme_cowplot() +
  theme(strip.background =element_blank(), 
        axis.text = element_text(size = 15), 
        strip.text = element_text(size = 20), 
        axis.title = element_text(size = 20)) +
  ylab("Occupancy") +
  xlab("Percent Agriculture") 


ggsave(main_trend_agri, filename = "plots/trends/main_env_agri.jpeg")


main_trend_neonic <- main_region_list %>% 
  rbindlist() %>% 
  left_join(region_nice_name) %>% 
  filter(env == "neonic") %>% 
  ggplot() +
  geom_line(aes(x = nn, y = mean)) +
  geom_ribbon(aes(x = nn, ymin = `X2.5.`, ymax = X97.5.), alpha = 0.3) +
  facet_wrap(~region_nice) +
  theme_cowplot() +
  theme(strip.background =element_blank(), 
        axis.text = element_text(size = 15), 
        strip.text = element_text(size = 20), 
        axis.title = element_text(size = 20)) +
  ylab("Occupancy") +
  xlab("Neonicotinoid Use") 


ggsave(main_trend_neonic, filename = "plots/trends/main_env_neonic.jpeg")


################ plot neonics for each species #######


create_data_for_plots_species <- function(region){
  
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
  
  env_list <- c('temp', 'prec', 'neonic', 'agriculture')
  
 
  for(env in env_list){
    print(env)
    
    # species by environment occupancy
    
    sp_occupancy <- lapply(1:nsp, FUN = function(x) get.y.val.all(sims.mat = sims.mat, ss = x, env, my.data = my.data))
    
    saveRDS(sp_occupancy, paste0('plots/int_data_plots/env/sp_',region,"_", env,'_occupancy.rds'))
    
    
    ## calculate genus occupancy 
    
    gn <- names(table(species_directory$genus))[table(species_directory$genus) > 5]
    
    genus_occupancy <- lapply(1:length(gn), FUN = function(x) get.y.val.all.genus(sims.mat = sims.mat, gg = gn[x], env, my.data = my.data, species_directory = species_directory))
    
    saveRDS(genus_occupancy, paste0('plots/int_data_plots/env/gn_',region,"_", env,'_occupancy.rds'))
    
  }
  
}



region_v <- c("West", "Center","SouthEast", "NorthEast")

lapply(region_v, create_data_for_plots_species)



### create environment region spaguetti plots per species ### 


region_env <- expand_grid(region_v = c("West","Center","NorthEast", "SouthEast"), env = c('temp', 'prec', 'neonic', 'agriculture'))

for(re in 1:nrow(region_env)){
  
  ## load data
  
  my.data <- readRDS(paste0("clean_data/data_prepared/my_data_env_genus_counties_1995_2015_ALL_", region_env$region_v[re],".rds"))
  
  species_directory <- data.frame(finalName = my.data$sp) %>% 
    mutate(ss = 1:n()) %>% 
    left_join(my.data[[2]])
  
  sp_occupancy  <- readRDS(paste0('plots/int_data_plots/env/sp_',region_env$region_v[re],"_", region_env$env[re],'_occupancy.rds')) %>% 
    rbindlist() %>% 
    left_join(species_directory)
  
  genus_occupancy  <- readRDS(paste0('plots/int_data_plots/env/gn_',region_env$region_v[re],"_", region_env$env[re],'_occupancy.rds')) %>% 
    rbindlist() %>% 
    rename(genus = gg)
  
  sp_occu_fil <- sp_occupancy %>% 
    filter(genus %in% genus_occupancy$genus)
  
  ## plot species only for genera with + 5 species
  
 env_all <- data.frame(env = c('temp', 'prec', 'neonic', 'agriculture'), names_nice = 
               c("Temperature", "Precipitation", "Neonicotinoid use", "Agricultural cover"), 
             colname_df = c("tt1", "pp", "nn", "ag"))
  
  sp_genus_plot <- ggplot() +
    geom_line(data = sp_occu_fil, aes_string(x = env_all$colname_df[env_all$env == region_env$env[re]], y = "mean", group= "finalName"), colour = 'grey') +
    facet_wrap(~genus) +
    geom_line(data = genus_occupancy, aes_string(x = env_all$colname_df[env_all$env == region_env$env[re]], y = "mean"), colour = 'black') +
    geom_ribbon(data = genus_occupancy, aes_string(x = env_all$colname_df[env_all$env == region_env$env[re]], ymin = "X2.5.", ymax = "X97.5."), alpha = 0.3) +
    theme_cowplot() +
    theme(strip.background =element_blank(), 
          axis.text = element_text(angle = 90)) +
    ylab("Occupancy") +
    xlab(env_all$names_nice[env_all$env == region_env$env[re]]) +
    ggtitle(region_env$region_v[re])
  
  ggsave(sp_genus_plot, filename = paste0("plots/trends/sp_env_",region_env$env[re], "_", region_env$region_v[re],".jpeg"))
  
}



################### just neonic results for the four regions ####

region_v <- c("West", "Center","SouthEast", "NorthEast")

mu.psi.all <- list()

for(region in region_v){
  
  family <- "ALL"
  year_range <- c(1995, 2015)
  
  # env era
  model <- "ms_env_area_2"
  
  model <- "ms_env_area_2_uncons"
  
  res <- readRDS(paste0("model_outputs/env/res_counties_", paste0(year_range, collapse = "_"), "_",model,"_", family, "_", region, ".rds"))
  
  res.summary <- readRDS(paste0("model_outputs/env/res.summary_counties_", paste0(year_range, collapse = "_"), "_",model,"_", family, "_", region, ".rds"))
  
  my.data <- readRDS(paste0("clean_data/data_prepared/my_data_env_genus_counties_", paste0(year_range, collapse = "_"),  "_", family, "_", region, ".rds"))
  
  
  vars <- rownames(res.summary$psrf$psrf)
  summ <- get.summ(vars, res.summary)
  
  summ.paper <- summ[str_detect(rownames(summ), 'mu.psi'),]
  
  mu.psi.all[[region]] <- summ.paper %>% 
    data.frame() %>% 
    tibble::rownames_to_column("Variable") 
}
  



data.frame(region = c('West', "Center", "SouthEast", "NorthEast"), 
           region_nice = c('West', "Center", "South East", "North East"))


mu.psi.all %>% 
  map_df(~data.frame(.x), .id = 'region') %>% 
  left_join(data.frame(region = c('West', "Center", "SouthEast", "NorthEast"), 
                       region_nice = c('West', "Center", "South East", "North East"))) %>% 
  filter(!Variable %in% c("mu.psi.0", "mu.psi.tmax2")) %>% 
  ggplot() +
  facet_wrap(~Variable, scales = 'free') +
  geom_point(aes(x = mean, y = region_nice)) + 
  geom_errorbarh(aes(xmin = `X2.5.`, xmax = `X97.5.`, y = region_nice), height = 0) + 
  geom_vline(xintercept = 0, colour = 'grey', linetype = 'dashed') +
  theme_cowplot() +
  xlab("Slope") + ylab("")  +
  theme(strip.background = element_blank())

  
