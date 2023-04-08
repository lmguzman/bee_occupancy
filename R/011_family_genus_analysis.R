library(ggplot2)
library(cowplot)
library(tidyr)
library(dplyr)
library(RColorBrewer)
library(stringr)
library(data.table)
library(purrr)
library(taxize)
source("R/src/initialize.R")

region_v <- c("South_EastFALSE", "CentralFALSE","Basin_and_RangeFALSE", 
              "Fruitful_RimFALSE", "Northern_CrescentFALSE", "Northern_Great_PlainsFALSE")

pest <- "_both"
pest <- ""
year_range <- c(1995, 2015)
family <- "ALL"

reg <- region_v[1]


for(reg in region_v){
  
  ## read occupancy
  occ_area <- readRDS(paste0("plots/area_neonics/int_data",pest,"/", reg, ".rds"))
  
  ## read data
  
  my.data <- readRDS(paste0("clean_data/data_prepared/my_data_env_genus_filtered_trait_agriregion_", paste0(year_range, collapse = "_"),  "_", family, "_", reg, ".rds"))
  
  species_directory <- data.frame(finalName = my.data$sp) %>% 
    mutate(ss = 1:n()) %>% 
    left_join(my.data[[2]])

  all_sp <- occ_area %>% 
    rbindlist() %>% 
    left_join(species_directory, by = c('species' = 'finalName')) 
  
  genus_area_mean <- split(all_sp, all_sp$genus) %>% 
    map(~select(.x, area_percent, species)) %>% 
    map(~mutate(.x, iter = rep(1:300, n()/300))) %>% 
    map(~pivot_wider(.x, names_from =species, values_from = area_percent)) %>% 
    map(~select(.x, -iter)) %>% 
    map(~rowMeans(.x))
    
  genus_area_summary <- do.call(rbind, lapply(genus_area_mean, function(x) c(mean(x), quantile(x, c(0.025, 0.975)))))
  
  colnames(genus_area_summary) <- c("mean", "bci2.5", "bci9.75")
  
  nsp_genus <- species_directory %>% 
    group_by(genus) %>% 
    summarise(n_sp = n())
  
  genus_area_summary_df <- genus_area_summary %>% 
    data.frame() %>% 
    tibble::rownames_to_column('genus') %>% 
    arrange(desc(mean)) %>% 
    left_join(nsp_genus)
    
  genus_area_summary_df
  
}

genus_area_summary_df %>% 
  mutate(genus = factor(genus, levels = genus_area_summary_df$genus)) %>% 
  ggplot() +
  geom_point(aes(x = mean, y = genus)) +
  geom_errorbarh(aes(xmin = bci9.75, xmax = bci2.5, y = genus), height = 0) +
  geom_vline(xintercept = 0, colour = 'grey', linetype = 'dashed') +
  geom_text(aes(label = n_sp, y = genus, x = max(bci9.75) + 0.1)) +
  theme_cowplot() +
  xlab("Mean Percent Area Lost") + ylab("")  +
  theme(strip.background = element_blank()) 
