library(ggplot2)
library(cowplot)
library(tidyr)
library(dplyr)
library(RColorBrewer)
library(stringr)
library(data.table)
library(purrr)
source("R/src/initialize.R")
source("R/src/plot_era_functions.R")


############## main trends ###########

## make function to load the data and calculate the main trends for all regions 

create_data_for_plots <- function(region){
  
  family <- "ALL"
  year_range <- c(1995, 2015)
  
  # load model output and initial data 
  
  model <- "ms_era_1_area"
  
  res <- readRDS(paste0("model_outputs/era/res_counties_", paste0(year_range, collapse = "_"), "_",model,"_", family, "_", region, ".rds"))
  
  res.summary <- readRDS(paste0("model_outputs/era/res.summary_counties_", paste0(year_range, collapse = "_"), "_",model,"_", family, "_", region, ".rds"))
  
  my.data <- readRDS(paste0("clean_data/data_prepared/my_data_era_genus_counties_", paste0(year_range, collapse = "_"),  "_", family, "_", region, ".rds"))
  
  ## calculate summary outputs and write them
  
  vars <- rownames(res.summary$psrf$psrf)
  summ <- get.summ(vars, res.summary)
  
  summ.paper <- summ[str_detect(rownames(summ), 'mu.psi'),]
  
  ncols_sum <- dim(summ.paper)[2]
  nrows_sum <- dim(summ.paper)[1]
  
  write(region,file="summary_outputs/main_era.txt",append=TRUE)
  write(c("            ",colnames(summ.paper)),,file="summary_outputs/main_era.txt", append=TRUE, ncolumns = (ncols_sum + 1))

  for(i in 1:nrows_sum){
    write(paste(rownames(summ.paper)[i], paste(summ.paper[i,], collapse = " ")),file="summary_outputs/main_era.txt",append=TRUE)
  }
  
 
  ### assign correct chains ##
  
  sims.mat <- do.call(rbind, res$mcmc)
  sims.arr <-
    aperm(sapply(res$mcmc, I, simplify='array'), c(1,3,2))
  
  ## get main data for occupancy estimation
  
  nera <- my.data[[1]]$nyr
  nsp <- my.data[[1]]$nsp
  species_directory <- my.data[[2]] %>% 
    mutate(sp_n = 1:n())
  
  ## calcualte main occupancy
  
  main_occupancy <- get.y.val.main.all(sims.mat = sims.mat)
  
  ## save
  
    saveRDS(main_occupancy, paste0('plots/int_data_plots/era/main_',region,'_occupancy.rds'))
  
}

## apply function to estimate occupancy for all regions

region_v <- c("West","Center","NorthEast", "SouthEast")

lapply(region_v, create_data_for_plots)

## read data of estimated occupancy for all regions

main_region_list <- list()

for(reg in region_v){
  
  main_region_list[[reg]] <- readRDS(paste0('plots/int_data_plots/era/main_',reg,'_occupancy.rds')) %>% 
    mutate(region = reg) %>% 
    as.data.table()
  
}

## plot the main trends for all regions 

region_nice_name <- data.frame(region = region_v, region_nice = c("West","Center","North East", "South East"))

main_trend_all <- main_region_list %>% 
  rbindlist() %>% 
  left_join(region_nice_name) %>% 
  ggplot() +
  geom_line(aes(x = yr, y = mean)) +
  geom_ribbon(aes(x = yr, ymin = `X2.5.`, ymax = X97.5.), alpha = 0.3) +
  facet_wrap(~region_nice) +
  theme_cowplot() +
  theme(strip.background =element_blank()) +
  ylab("Occupancy") +
  xlab("") +
  scale_x_continuous(breaks = c(1:7), labels = seq(1995, 2015, 3))
  

ggsave(main_trend_all, filename = "plots/trends/main_era.jpeg")



############# Species specific trends ##########


create_data_for_plots_species <- function(region){
  
  family <- "ALL"
  year_range <- c(1995, 2015)
  
  # load model output and initial data 
  
  model <- "ms_era_1_area"
  
  res <- readRDS(paste0("model_outputs/era/res_counties_", paste0(year_range, collapse = "_"), "_",model,"_", family, "_", region, ".rds"))
  
  res.summary <- readRDS(paste0("model_outputs/era/res.summary_counties_", paste0(year_range, collapse = "_"), "_",model,"_", family, "_", region, ".rds"))
  
  my.data <- readRDS(paste0("clean_data/data_prepared/my_data_era_genus_counties_", paste0(year_range, collapse = "_"),  "_", family, "_", region, ".rds"))
  
  ## calculate summary outputs and write them
  
  vars <- rownames(res.summary$psrf$psrf)
  summ <- get.summ(vars, res.summary)
  
  summ.paper <- summ[str_detect(rownames(summ), 'psi.era'),]
  
  rownames(summ.paper) <- my.data$sp

  ncols_sum <- dim(summ.paper)[2]
  nrows_sum <- dim(summ.paper)[1]
  
  write(region,file=paste0("summary_outputs/sp_era_",region,".txt"))
  write(c("            ",colnames(summ.paper)),,file=paste0("summary_outputs/sp_era_",region,".txt"), append=TRUE, ncolumns = (ncols_sum + 1))
  
  for(i in 1:nrows_sum){
    write(paste(rownames(summ.paper)[i], paste(summ.paper[i,], collapse = " ")),file=paste0("summary_outputs/sp_era_",region,".txt"),append=TRUE)
  }
  
  
  ### assign correct chains ##
  
  sims.mat <- do.call(rbind, res$mcmc)
  sims.arr <-
    aperm(sapply(res$mcmc, I, simplify='array'), c(1,3,2))
  
  ## get main data for occupancy estimation
  
  nera <- my.data[[1]]$nyr
  nsp <- my.data[[1]]$nsp
  
  species_directory <- data.frame(finalName = my.data$sp) %>% 
    mutate(sp_n = 1:n()) %>% 
    left_join(my.data[[2]])
  
  ## calcualte species occupancy
  
  sp_yr <- expand.grid(sp = 1:nsp, yr = 1:nera)
  
  sp_occupancy <- lapply(1:nrow(sp_yr), FUN = function(x) get.y.val(sims.mat = sims.mat, ss = sp_yr$sp[x], yr = sp_yr$yr[x]))
  
  ## save
  
  saveRDS(sp_occupancy, paste0('plots/int_data_plots/era/sp_',region,'_occupancy.rds'))
  
  ## calculate genus occupancy 

  gn_yr <- expand.grid(gn = names(table(species_directory$genus))[table(species_directory$genus) > 5], yr = 1:nera)
  
  genus_occupancy <- lapply(1:nrow(gn_yr), FUN = function(x) get.y.val.genus(sims.mat = sims.mat, gg = gn_yr$gn[x], yr = gn_yr$yr[x], species_directory = species_directory))
  
  ## save
  
  saveRDS(genus_occupancy, paste0('plots/int_data_plots/era/genus_',region,'_occupancy.rds'))
}

## apply function to estimate occupancy for all regions

region_v <- c("West","Center","NorthEast", "SouthEast")

lapply(region_v, create_data_for_plots_species)


########## plot main era trends for every species and genus ######

region_v <- c("West","Center","NorthEast", "SouthEast")

for(reg in region_v){
  
  ## load data
  
  my.data <- readRDS(paste0("clean_data/data_prepared/my_data_era_genus_counties_1995_2015_ALL_", reg,".rds"))
  
  species_directory <- data.frame(finalName = my.data$sp) %>% 
    mutate(ss = 1:n()) %>% 
    left_join(my.data[[2]])
  
 sp_occupancy  <- readRDS(paste0('plots/int_data_plots/era/sp_',reg,'_occupancy.rds')) %>% 
   rbindlist() %>% 
   left_join(species_directory)
 
 genus_occupancy  <- readRDS(paste0('plots/int_data_plots/era/genus_',reg,'_occupancy.rds')) %>% 
   rbindlist() %>% 
   rename(genus = gg)
 
 sp_occu_fil <- sp_occupancy %>% 
   filter(genus %in% genus_occupancy$genus)
 
 
 ## identify species that have extreme decline or increases
 
 species_large_change <- sp_occu_fil %>% 
   select(yr, mean, finalName) %>% 
   filter(yr %in% c(1, 7)) %>% 
   pivot_wider(names_from = yr, values_from = mean) %>% 
   mutate(per_dif = abs((`1`-`7`)/(`1`)), dif = `7`-`1`, abs_dif = abs(dif)) %>% 
   arrange(desc(per_dif)) %>% 
   filter(per_dif > 0.75) %>% 
   select(finalName, dif, abs_dif) %>% 
   mutate(plot_text = finalName) %>%
   separate(plot_text, c("gn", "sp"), " ") %>% 
   mutate(plot_text = paste(str_sub(gn, 1, 1),sp, round(dif, 2))) %>% 
   left_join(species_directory) 
 
 genus_mult_species <- table(species_large_change$genus)[table(species_large_change$genus) > 1]
 
 species_large_change_loc <- species_large_change %>% 
   mutate(x = 4, y = 0.2) %>% 
   mutate(y = ifelse(genus %in% names(genus_mult_species), runif(sum(genus_mult_species), 0,0.5), y))
 
 ## plot species only for genera with + 5 species

sp_genus_plot <- ggplot() +
   geom_line(data = sp_occu_fil, aes(x = yr, y = mean, group= finalName), colour = 'grey') +
   facet_wrap(~genus) +
   geom_line(data = genus_occupancy, aes(x = yr, y = mean), colour = 'black') +
  geom_ribbon(data = genus_occupancy, aes(x = yr, ymin = `X2.5.`, ymax = X97.5.), alpha = 0.3) +
  theme_cowplot() +
  theme(strip.background =element_blank(), 
        axis.text = element_text(angle = 90)) +
  ylab("Occupancy") +
  xlab("") +
  scale_x_continuous(breaks = c(1:7), labels = seq(1995, 2015, 3))

ggsave(sp_genus_plot, filename = paste0("plots/trends/sp_era_",reg,".jpeg"))

#### plot the same but with species labels with large increases or decreases 
sp_genus_plot_text <- ggplot() +
  geom_line(data = sp_occu_fil, aes(x = yr, y = mean, group= finalName), colour = 'grey') +
  facet_wrap(~genus) +
  geom_text(data = species_large_change_loc, aes(x = x, y = y, label= plot_text), size = 2.5, check_overlap = TRUE) +
  geom_line(data = genus_occupancy, aes(x = yr, y = mean), colour = 'black') +
  geom_ribbon(data = genus_occupancy, aes(x = yr, ymin = `X2.5.`, ymax = X97.5.), alpha = 0.3) +
  theme_cowplot() +
  theme(strip.background =element_blank(), 
        axis.text = element_text(angle = 90)) +
  ylab("Occupancy") +
  xlab("") +
  ggtitle(reg) +
  scale_x_continuous(breaks = c(1:7), labels = seq(1995, 2015, 3))

ggsave(sp_genus_plot_text, filename = paste0("plots/trends/sp_name_era_",reg,".jpeg"))

}


 