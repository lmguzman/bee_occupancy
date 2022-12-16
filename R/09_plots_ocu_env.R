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
  

    
    # main trend occupancy
    
    sp_yr <- expand.grid(yr = 1:nera, sp = 1:nsp)
  
    main_occupancy <- lapply(1:nrow(sp_yr), FUN = function(x) get.y.val.all(sims.mat = sims.mat, ss = sp_yr$sp[x], my.data = my.data,
                                                         yr = sp_yr$yr[x], species_directory = species_directory, region = region))

    env_main_occupancy <- rbindlist(main_occupancy)

    saveRDS(env_main_occupancy, paste0('plots/int_data_plots/env/sp_env_occ_',region,'_occupancy.rds'))

}


region_v <- c("West", "Center","SouthEast", "NorthEast")

lapply(region_v, create_data_for_plots)


### plot change in occupancy for every species ## 

region_v <- c("West","Center","NorthEast", "SouthEast")

for(reg in region_v){
  
  ## load data
  
  my.data <- readRDS(paste0("clean_data/data_prepared/my_data_env_genus_counties_1995_2015_ALL_", reg,".rds"))
  
  species_directory <- data.frame(finalName = my.data$sp) %>% 
    mutate(ss = 1:n()) %>% 
    left_join(my.data[[2]])
    
  all_chains <- list.files("plots/int_data_plots/env/chains/", full.names = TRUE)
  
  chains_region <- all_chains[str_detect(all_chains, reg)]
  
  sp_yr <- str_extract(chains_region, "\\d+\\_\\d+")
  
  chains_loaded <- lapply(chains_region, readRDS)
  
  chains_mat <- do.call(cbind, chains_loaded)
  
  colnames(chains_mat) <- sp_yr
  
  genera <- unique(species_directory$genus)
  
  genus_chain_all <- function(species_directory, chains_mat, species_genus, g, yr){
    
    species_genus <- species_directory %>% 
      filter(genus == genera[g]) %>% 
      select(ss) %>% unlist()
    
    genus_chain <- rowMeans(chains_mat[,paste0(species_genus, "_", yr), drop = FALSE])
    
    data.table(data.frame(g, yr, mean=mean(genus_chain), t(quantile(genus_chain, probs=c(0.025,0.975)))))
  }
  
  genus_yr_all <- expand.grid(genus = 1:length(genera), yr = 1:my.data[[1]]$nyr)
  
  genus_chains_year <- lapply(1:nrow(genus_yr_all), FUN = function(x) genus_chain_all(species_directory, 
                                                                  chains_mat, species_genus, 
                                                                  g = genus_yr_all$genus[x], yr = genus_yr_all$yr[x]))

  genus_chain_df <- rbindlist(genus_chains_year)  
  
  saveRDS(genus_chain_df, paste0("plots/int_data_plots/env/gn_env_occ",reg,"_occupancy.rds"))
  
}




### plot change in occupancy for every genus using the environmental occupancy  ## 

region_v <- c("West","Center","NorthEast", "SouthEast")

for(reg in region_v){
  
  ## load data
  
  my.data <- readRDS(paste0("clean_data/data_prepared/my_data_env_genus_counties_1995_2015_ALL_", reg,".rds"))
  
  species_directory <- data.frame(finalName = my.data$sp) %>% 
    mutate(ss = 1:n()) %>% 
    left_join(my.data[[2]]) %>% 
    left_join(data.frame(genus = unique(species_directory$genus), g = 1:length(unique(species_directory$genus))))

  sp_occupancy  <- readRDS(paste0('plots/int_data_plots/env/sp_env_occ_',reg,'_occupancy.rds')) %>% 
    left_join(species_directory)
  
  genus_occupancy  <- readRDS(paste0('plots/int_data_plots/env/gn_env_occ',reg,'_occupancy.rds')) %>% 
    left_join(species_directory)
  
  genus_5 <- species_directory %>%
    group_by(genus) %>%
    summarise(n = n()) %>%
    filter(n>5) %>%
    select(genus) %>% unlist()
  
  sp_occu_fil <- sp_occupancy %>% 
    filter(genus %in% genus_5)
  
  genus_occu_fil <- genus_occupancy %>% 
    filter(genus %in% genus_5)
  
  #### plot the same but with species labels with large increases or decreases 
  sp_genus_plot_text <- ggplot() +
    geom_line(data = sp_occu_fil, aes(x = yr, y = mean, group= finalName), colour = 'grey') +
    geom_line(data = genus_occu_fil, aes(x = yr, y = mean), colour = 'black') +
    geom_ribbon(data = genus_occu_fil, aes(x = yr, ymin = `X2.5.`, ymax = X97.5.), alpha = 0.3) +
    facet_wrap(~genus) +
    theme_cowplot() +
    theme(strip.background =element_blank(), 
          axis.text = element_text(angle = 90)) +
    ylab("Occupancy") +
    xlab("") +
    ggtitle(reg) +
    scale_x_continuous(breaks = c(1:7), labels = seq(1995, 2015, 3))
  
  ggsave(sp_genus_plot_text, filename = paste0("plots/trends/sp_env_ocu_",reg,".jpeg"))
  
}
