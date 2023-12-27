###### Script to produce figures that agregate at the genus level ######

### Figures 3, S10, S11, S12, S13, S14

library(ggplot2)
library(cowplot)
library(tidyr)
library(dplyr)
library(RColorBrewer)
library(stringr)
library(data.table)
library(purrr)
library(taxize)
library(forcats)
source("R/src/initialize.R")
source("R/src/plot_env_functions.R")

## load genus to family assignments 

species_genus_family <- read.csv("clean_data/native_expected/species_genus_family.csv") 

### extract data from model outputs ### 

species_results_ag <- list()
species_results_honey <- list()
species_results_pest <- list()

mcmc_sp_apa <- list()
mcmc_sp_honey <- list()
mcmc_sp_pest <- list()

fam <- c("Andrenidae", "Apidae", "Halictidae", "Megachilidae",  "Colletidae|Melittidae")

obs_genus <- list()

for(f in fam){
  
  ## read data
  
  my.data <- readRDS(paste0("clean_data/data_prepared/my_data_env_genus_filtered_trait_agriregion_both_pest_area_county_1995_2015_",f,"_ALLFALSE.rds"))
  
  species_directory <- data.frame(finalName = my.data$sp) %>% 
    mutate(ss = 1:n()) %>% 
    left_join(my.data[[2]])
  
  observations <- read.csv(paste0("clean_data/observations_used/",f,".csv"))
  
  obs_genus[[f]] <- observations %>% 
    group_by(genus) %>% 
    count()
  
  ## apa model 
  
  res_summary_apa <- readRDS(paste0("model_outputs/res.summary_genus_filtered_agriregion_pest_area_county_both_1995_2015_ms_area_climate_canag_17_",f,"_ALLFALSE.rds"))
  
  vars <- rownames(res_summary_apa$psrf$psrf)
  summ_apa <- get.summ(vars, res_summary_apa)
  
  mcmc_apa <- rbind(res_summary_apa$mcmc[[1]], res_summary_apa$mcmc[[2]], res_summary_apa$mcmc[[3]])
  
  ## honey bee model
  
  res_summary_honey <- readRDS(paste0("model_outputs/res.summary_genus_filtered_agriregion_pest_area_county_both_1995_2015_ms_area_honeytime_canag_16_",f,"_ALLFALSE.rds"))
  
  vars <- rownames(res_summary_honey$psrf$psrf)
  summ_honey <- get.summ(vars, res_summary_honey)
  
  mcmc_honey <- rbind(res_summary_honey$mcmc[[1]], res_summary_honey$mcmc[[2]], res_summary_honey$mcmc[[3]])
  
  ## pesticide model
  
  res_summary_pesticide <- readRDS(paste0("model_outputs/res.summary_genus_filtered_agriregion_pest_area_county_both_1995_2015_ms_area_honeytime_pestar_canag_15_",f,"_ALLFALSE.rds"))
  
  vars <- rownames(res_summary_pesticide$psrf$psrf)
  summ_pesticide <- get.summ(vars, res_summary_pesticide)
  
  mcmc_pest <- rbind(res_summary_pesticide$mcmc[[1]], res_summary_pesticide$mcmc[[2]], res_summary_pesticide$mcmc[[3]])
  
  
  ##### species level results #####
  
  ## apa
  
  species_results_ag[[f]] <- summ_apa[str_detect(rownames(summ_apa), 'psi.canag\\['),] %>% 
    as.data.frame() %>% 
    tibble::rownames_to_column("psi.canag") %>% 
    mutate(ss = as.numeric(str_extract(str_extract(psi.canag, "\\[\\d+\\]"), "\\d+"))) %>% 
    left_join(species_directory) %>% 
    left_join(species_genus_family)
  
  ## honey
  
  species_results_honey[[f]] <- summ_honey[str_detect(rownames(summ_honey), 'psi.col\\['),] %>% 
    as.data.frame() %>% 
    tibble::rownames_to_column("psi.col") %>% 
    mutate(ss = as.numeric(str_extract(str_extract(psi.col, "\\[\\d+\\]"), "\\d+"))) %>% 
    left_join(species_directory) %>% 
    left_join(species_genus_family)
  
  ## pesticide
  
  species_results_pest[[f]] <- summ_pesticide[str_detect(rownames(summ_pesticide), 'psi.pest1\\['),] %>% 
    as.data.frame() %>% 
    tibble::rownames_to_column("psi.pest") %>% 
    mutate(ss = as.numeric(str_extract(str_extract(psi.pest, "\\[\\d+\\]"), "\\d+"))) %>% 
    left_join(species_directory) %>% 
    left_join(species_genus_family)
  
  ######### genus summaries ##########
  
  ## apa
  
  mcmc_sp_apa[[f]] <- mcmc_apa[,str_detect(colnames(mcmc_apa), 'psi.canag\\[')] %>% 
    as.data.frame(row.names = 1:nrow(.)) %>% 
    t() %>% 
    as.data.frame() %>% 
    janitor::clean_names() %>% 
    tibble::rownames_to_column("psi.apa") %>% 
    mutate(ss = as.numeric(str_extract(str_extract(psi.apa, "\\[\\d+\\]"), "\\d+"))) %>% 
    left_join(species_directory) 
  
  
  ## honey
  
  mcmc_sp_honey[[f]] <- mcmc_honey[,str_detect(colnames(mcmc_honey), 'psi.col\\[')] %>% 
    as.data.frame(row.names = 1:nrow(.)) %>% 
    t() %>% 
    as.data.frame() %>% 
    janitor::clean_names() %>% 
    tibble::rownames_to_column("psi.honey") %>% 
    mutate(ss = as.numeric(str_extract(str_extract(psi.honey, "\\[\\d+\\]"), "\\d+"))) %>% 
    left_join(species_directory)
  
  ## pesticide
  
  mcmc_sp_pest[[f]] <- mcmc_pest[,str_detect(colnames(mcmc_pest), 'psi.pest1\\[')] %>% 
    as.data.frame(row.names = 1:nrow(.)) %>% 
    t() %>% 
    as.data.frame() %>% 
    janitor::clean_names() %>% 
    tibble::rownames_to_column("psi.pest") %>% 
    mutate(ss = as.numeric(str_extract(str_extract(psi.pest, "\\[\\d+\\]"), "\\d+"))) %>% 
    left_join(species_directory)
  
}

#### species plots Figure S12 ###

## set the color palette

n_families <- length(unique(species_genus_family$family))

palette1_named = setNames(object = scales::hue_pal()(n_families), nm = sort(unique(species_genus_family$family)))

species_ag_plot <- do.call(rbind, species_results_ag) %>% 
  mutate(genus = fct_reorder(genus, desc(genus))) %>% 
  ggplot() +
  geom_point(aes(x = mean, y = genus, colour = family)) +
  theme_cowplot() +
  geom_vline(xintercept = 0, linetype="dashed", colour = 'grey') +
  ylab('') +
  xlab("Effect of Animal Pollinated Agriculture") +
  theme(legend.position = 'none') +
  scale_colour_manual(values = palette1_named)

species_honey_plot <- do.call(rbind, species_results_honey) %>% 
  mutate(genus = fct_reorder(genus, desc(genus))) %>% 
  ggplot() +
  geom_point(aes(x = mean, y = genus, colour = family)) +
  theme_cowplot() +
  geom_vline(xintercept = 0, linetype="dashed", colour = 'grey') +
  ylab('') +
  xlab("Effect of Honey Bees") +
  theme(legend.position = 'none') +
  scale_colour_manual(values = palette1_named)

species_pest_plot <- do.call(rbind, species_results_pest) %>% 
  mutate(genus = fct_reorder(genus, desc(genus))) %>% 
  ggplot() +
  geom_point(aes(x = mean, y = genus, colour = family)) +
  theme_cowplot() +
  geom_vline(xintercept = 0, linetype="dashed", colour = 'grey') +
  ylab('') +
  xlab("Effect of Pesticide Use") +
  theme(legend.position = 'none') +
  scale_colour_manual(values = palette1_named)

species_plot_all <- plot_grid(species_pest_plot, species_ag_plot, species_honey_plot,  labels = c("A.", "B.", "C."), nrow = 1)

ggsave(species_plot_all, file = 'plots/species_averages.pdf', width = 12, height = 15)


######### genus plot all Figure S11  ######

palette1_significance = setNames(object = c('#808080', "#5D3A9B","#E66100"), nm = c('not_sig', "sig_pos", "sig_neg"))

## apa

mcmc_apa_all <- do.call(rbind, mcmc_sp_apa) 

genus_estimates_apa <- split(mcmc_apa_all, mcmc_apa_all$genus) %>% 
  map(~dplyr::select(.x, -psi.apa, -ss, -finalName, -genus)) %>% 
  map(~colMeans(.x)) %>% 
  map_df(~data.frame(mean = mean(.x), x25= quantile(.x, 0.025), x975 = quantile(.x, 0.975)), .id = 'genus') %>% 
  left_join(unique(dplyr::select(species_genus_family, genus, family))) %>% 
  mutate(genus = fct_reorder(genus, desc(genus))) %>% 
  mutate(color_pat = case_when(x25 < 0 & x975 < 0 ~ "sig_neg",
                               x25 > 0 & x975 > 0 ~ "sig_pos",
                               TRUE ~ "not_sig"))

genus_averages_plots_apa <- ggplot() +
  geom_point(data = genus_estimates_apa, aes(x = mean, y = genus, colour = color_pat)) +
  geom_errorbarh(data = genus_estimates_apa, aes(xmin = x25, xmax = x975, y = genus, colour = color_pat), height = 0) +
  theme_cowplot() +
  geom_vline(xintercept = 0, linetype="dashed", colour = 'grey') +
  ylab('') +
  xlab("Effect of Animal Pollinated Agriculture") +
  theme(legend.position = 'none',
        axis.text.y = element_text(size = 9))+
  scale_colour_manual(values = palette1_significance)


# honey

mcmc_honey_all <- do.call(rbind, mcmc_sp_honey) 

genus_estimates_honey <- split(mcmc_honey_all, mcmc_honey_all$genus) %>% 
  map(~dplyr::select(.x, -psi.honey, -ss, -finalName, -genus)) %>% 
  map(~colMeans(.x)) %>% 
  map_df(~data.frame(mean = mean(.x), x25= quantile(.x, 0.025), x975 = quantile(.x, 0.975)), .id = 'genus') %>% 
  left_join(unique(dplyr::select(species_genus_family, genus, family))) %>% 
  mutate(genus = fct_reorder(genus, desc(genus))) %>% 
  mutate(color_pat = case_when(x25 < 0 & x975 < 0 ~ "sig_neg",
                               x25 > 0 & x975 > 0 ~ "sig_pos",
                               TRUE ~ "not_sig"))

genus_averages_plots_honey <- ggplot() +
  geom_point(data = genus_estimates_honey, aes(x = mean, y = genus, colour = color_pat)) +
  geom_errorbarh(data = genus_estimates_honey, aes(xmin = x25, xmax = x975, y = genus, colour = color_pat), height = 0) +
  theme_cowplot() +
  geom_vline(xintercept = 0, linetype="dashed", colour = 'grey') +
  ylab('') +
  xlab("Effect of Honey Bees") +
  theme(legend.position = 'none',
        axis.text.y = element_text(size = 9))+
  scale_colour_manual(values = palette1_significance)

# pesticide

mcmc_pest_all <- do.call(rbind, mcmc_sp_pest) 

genus_estimates_pest <- split(mcmc_pest_all, mcmc_pest_all$genus) %>% 
  map(~dplyr::select(.x, -psi.pest, -ss, -finalName, -genus)) %>% 
  map(~colMeans(.x)) %>% 
  map_df(~data.frame(mean = mean(.x), x25= quantile(.x, 0.025), x975 = quantile(.x, 0.975)), .id = 'genus') %>% 
  left_join(unique(dplyr::select(species_genus_family, genus, family))) %>% 
  mutate(genus = fct_reorder(genus, desc(genus))) %>% 
  mutate(color_pat = case_when(x25 < 0 & x975 < 0 ~ "sig_neg",
                               x25 > 0 & x975 > 0 ~ "sig_pos",
                               TRUE ~ "not_sig"))

genus_averages_plots_pest <- ggplot() +
  geom_point(data = genus_estimates_pest, aes(x = mean, y = genus, colour = color_pat)) +
  geom_errorbarh(data = genus_estimates_pest, aes(xmin = x25, xmax = x975, y = genus, colour = color_pat), height = 0) +
  theme_cowplot() +
  geom_vline(xintercept = 0, linetype="dashed", colour = 'grey') +
  ylab('') +
  xlab("Effect of Pesticide Use") +
  theme(legend.position = 'none',
        axis.text.y = element_text(size = 9))+
  scale_colour_manual(values = palette1_significance)


genus_plot_all <- plot_grid(genus_averages_plots_pest, genus_averages_plots_apa, genus_averages_plots_honey,  labels = c("A.", "B.", "C."), nrow = 1)

ggsave(genus_plot_all, file = 'plots/genus_averages.pdf', width = 12, height = 15)


##### Figure 3 genus with more than 10 species #########

## apa

n_obs <- obs_genus %>% 
  map_df(~as.data.frame(.x))

genus_g10_apa <-mcmc_apa_all %>% 
  select(finalName, genus) %>% 
  group_by(genus) %>% 
  count() %>% 
  filter(n > 10)

genus_family <- select(species_genus_family, genus, family) %>% unique()

genus_10_plots_apa <- genus_estimates_apa %>% 
  filter(genus %in% genus_g10_apa$genus) %>% 
  inner_join(genus_family) %>% 
  left_join(n_obs) %>% 
  arrange(family, genus) %>% 
  ggplot() +
  facet_grid(rows = vars(family), 
             scales = "free_y", # Let the x axis vary across facets.
             space = "free_y",  # Let the width of facets vary and force all bars to have the same width.
             switch = "y") +
  geom_point(aes(x = mean, y = genus, colour = color_pat)) +
  geom_errorbarh(aes(xmin = x25, xmax = x975, y = genus, colour = color_pat), height = 0) +
  theme_cowplot() +
  geom_vline(xintercept = 0, linetype="dashed", colour = 'grey') +
  theme(legend.position = 'none',
        axis.text.y = element_text(size = 12), 
        strip.placement = "outside",                      # Place facet labels outside x axis labels.
        strip.background = element_rect(fill = "white"),  # Make facet label background white.
        axis.title.y = element_blank(),
        strip.text = element_text(color = 'white')) + 
  scale_colour_manual(values = palette1_significance) +
  scale_y_discrete(limits = rev) +
  xlab("Effect of Animal Pollinated Agriculture") 


## honey

genus_g10_honey <-mcmc_honey_all %>% 
  select(finalName, genus) %>% 
  group_by(genus) %>% 
  count() %>% 
  filter(n > 10)

genus_10_plots_honey <- genus_estimates_honey %>% 
  filter(genus %in% genus_g10_honey$genus) %>% 
  inner_join(genus_family) %>% 
  arrange(family, genus) %>% 
  ggplot() +
  facet_grid(rows = vars(family), 
             scales = "free_y", # Let the x axis vary across facets.
             space = "free_y",  # Let the width of facets vary and force all bars to have the same width.
             switch = "y") +
  geom_point(aes(x = mean, y = genus, colour = color_pat)) +
  geom_errorbarh(aes(xmin = x25, xmax = x975, y = genus, colour = color_pat), height = 0) +
  theme_cowplot() +
  geom_vline(xintercept = 0, linetype="dashed", colour = 'grey') +
  theme(legend.position = 'none',
        axis.text.y = element_text(size = 12), 
        strip.placement = "outside",                      # Place facet labels outside x axis labels.
        strip.background = element_rect(fill = "white"),  # Make facet label background white.
        axis.title.y = element_blank(),
        strip.text = element_text(color = 'white')) + 
  scale_colour_manual(values = palette1_significance) +
  scale_y_discrete(limits = rev) +
  xlab("Effect of Honey Bees") 
  

## pest

genus_g10_pest <-mcmc_pest_all %>% 
  select(finalName, genus) %>% 
  group_by(genus) %>% 
  count() %>% 
  filter(n > 10)

genus_10_plots_pest <- genus_estimates_pest %>% 
  filter(genus %in% genus_g10_pest$genus) %>% 
  inner_join(genus_family) %>% 
  left_join(n_obs) %>% 
  arrange(family, genus) %>% 
  ggplot() +
  facet_grid(rows = vars(family), 
             scales = "free_y", # Let the x axis vary across facets.
             space = "free_y",  # Let the width of facets vary and force all bars to have the same width.
             switch = "y") +
  geom_point(aes(x = mean, y = genus, colour = color_pat)) +
  geom_errorbarh(aes(xmin = x25, xmax = x975, y = genus, colour = color_pat), height = 0) +
  theme_cowplot() +
  geom_vline(xintercept = 0, linetype="dashed", colour = 'grey') +
  theme(legend.position = 'none',
        axis.text.y = element_text(size = 12),
        strip.placement = "outside",                      # Place facet labels outside x axis labels.
        strip.background = element_rect(fill = "white"),  # Make facet label background white.
        axis.title.y = element_blank()) +
  scale_colour_manual(values = palette1_significance) +
  scale_y_discrete(limits = rev) +
  xlab("Effect of Pesticide Use")

all_genus_g10_plots <- plot_grid(genus_10_plots_pest, genus_10_plots_apa, genus_10_plots_honey,  labels = c("A.", "B.", "C."), nrow = 1)

ggsave(all_genus_g10_plots, file = 'plots/Figure3.pdf', width = 12, height = 11)


########### Figure S10 occupancy trends at the family level ######

create_data_for_plots <- function(f){

  ## load model outputs and input data
  
  res.summary <- readRDS(paste0("model_outputs/res.summary_genus_filtered_agriregion_pest_area_county_both_1995_2015_ms_area_honeytime_pestar_canag_15_", f,"_ALLFALSE.rds"))
  
  my.data <- readRDS(paste0("clean_data/data_prepared/my_data_env_genus_filtered_trait_agriregion_both_pest_area_county_1995_2015_",f,"_ALLFALSE.rds"))
  
  ### re-organize model estimate chains ##
  
  sims.mat <- do.call(rbind, res.summary$mcmc)
  sims.arr <-
    aperm(sapply(res.summary$mcmc, I, simplify='array'), c(1,3,2))
  
  ### get correct data for species and year
  
  nera <- my.data[[1]]$nyr
  nsp <- my.data[[1]]$nsp
  species_directory <- my.data[[2]] %>% 
    mutate(sp_n = 1:n())
  
  # calculate the main occupancy across a family 
    
    main_occupancy <- get.y.val.main.all(sims.mat = sims.mat, my.data = my.data)
    
    saveRDS(main_occupancy, paste0('plots/int_data_plots/env/main_',f,'_occupancy.rds'))
  
}

## apply the above function for all families

fam <- c("Andrenidae", "Apidae", "Halictidae", 
         "Megachilidae", "Colletidae|Melittidae")

lapply(fam, create_data_for_plots)

## compile outputs

main_family_list <- list()

for(f in fam){
  
  main_family_list[[f]] <- readRDS(paste0('plots/int_data_plots/env/main_',f,'_occupancy.rds')) %>% 
    mutate(family = f) %>% 
    as.data.table()
  
}

#### Figure S10 #####

main_trend_pes <- main_family_list %>% 
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

ggsave(main_trend_pes, filename = "plots/trends/main_env_pesticide.pdf", width = 9)


### get summaries for results

# check minimum and maximum pesticide value is the same for all families 

main_family_list %>% 
  rbindlist() %>% 
  group_by(family) %>% 
  summarise(min(ps), max(ps))

main_pest_df <- main_family_list %>% 
  rbindlist()

main_pest_df %>% 
  filter(ps %in% c(min(main_pest_df$ps), max(main_pest_df$ps))) %>% 
  dplyr::select(mean, ps, family) %>% 
  pivot_wider(names_from = ps, values_from = mean) %>% 
  rename(no_pest = `-4.17560742453955`, max_pest = `1.77649633931698`) %>% 
  mutate(net_change = no_pest - max_pest, percent_change = (no_pest - max_pest)/no_pest)
 
############## Figure S13 #############
 
##### calculates occupancy for every genera in Model 1  #####

create_data_for_plots <- function(f){
  
  ## load model results and input data
  
  res.summary <- readRDS(paste0("model_outputs/res.summary_genus_filtered_agriregion_pest_area_county_both_1995_2015_ms_area_honeytime_pestar_canag_15_", f,"_ALLFALSE.rds"))
  
  my.data <- readRDS(paste0("clean_data/data_prepared/my_data_env_genus_filtered_trait_agriregion_both_pest_area_county_1995_2015_",f,"_ALLFALSE.rds"))
  
  ### assign correct chains ##
  
  sims.mat <- do.call(rbind, res.summary$mcmc)
  sims.arr <-
    aperm(sapply(res.summary$mcmc, I, simplify='array'), c(1,3,2))
  
  nera <- my.data[[1]]$nyr
  nsp <- my.data[[1]]$nsp
  species_directory <- my.data[[2]] %>% 
    mutate(sp_n = 1:n())
  
  # genus trend occupancy
  
  main_occupancy <- get.y.val.genus.all(sims.mat = sims.mat, my.data = my.data, species_directory = species_directory)
  
  saveRDS(main_occupancy, paste0('plots/int_data_plots/env/genus_',f,'_occupancy.rds'))
  
}

## apply the function across all families

fam <- c("Andrenidae", "Apidae", "Halictidae", 
         "Megachilidae", "Colletidae|Melittidae")

lapply(fam, create_data_for_plots)

## compile outputs

genus_family_list <- list()

for(f in fam){
  
  genus_family_list[[f]] <- readRDS(paste0('plots/int_data_plots/env/genus_',f,'_occupancy.rds')) %>% 
    mutate(family = f) %>% 
    as.data.table()
  
}

## plot Figure S13 ##

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

ggsave(genus_trend_pes, filename = "plots/trends/genus_env_pesticide.pdf", width = 9)

#### get genus level summaries ## 

# make sure every genera has the same min and max

genus_family_list %>% 
  rbindlist() %>% 
  group_by(family, gg) %>% 
  summarise(min(ps), max(ps))

genus_family_pest_df <- genus_family_list %>% 
  rbindlist()

## calculate percent change

genus_family_pest_df %>% 
  filter(ps %in% c(min(main_pest_df$ps), max(main_pest_df$ps))) %>% 
  dplyr::select(mean, ps, family, gg) %>% 
  pivot_wider(names_from = ps, values_from = mean) %>% 
  rename(no_pest = `-4.17560742453955`, max_pest = `1.77649633931698`) %>% 
  mutate(net_change = no_pest - max_pest, percent_change = 100*(no_pest - max_pest)/no_pest) %>% 
  arrange(percent_change) %>% View()



########## Figure S14 #########

##### get genus level occupancy estimates for animal pollinated agriculture  #####

create_data_for_plots <- function(f){
  
  ## load model outputs and data 
  
  res.summary <- readRDS(paste0("model_outputs/res.summary_genus_filtered_agriregion_pest_area_county_both_1995_2015_ms_area_climate_canag_17_", f,"_ALLFALSE.rds"))
  
  my.data <- readRDS(paste0("clean_data/data_prepared/my_data_env_genus_filtered_trait_agriregion_both_pest_area_county_1995_2015_",f,"_ALLFALSE.rds"))
  
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

## apply for all families 

fam <- c("Andrenidae", "Apidae", "Halictidae", 
         "Megachilidae", "Colletidae|Melittidae")

lapply(fam, create_data_for_plots)


## compile outputs

genus_family_list <- list()

for(f in fam){
  
  genus_family_list[[f]] <- readRDS(paste0('plots/int_data_plots/env/genus_',f,'_occupancy_canag.rds')) %>% 
    mutate(family = f) %>% 
    as.data.table()
  
}

#### Figure S14 ###

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


ggsave(genus_trend_canag, filename = "plots/trends/genus_env_canag.pdf", width = 9)

#### get summary values for results ##

## get minimum and maximum for every family 

min_max_family <- genus_family_list %>% 
  rbindlist() %>% 
  group_by(family) %>% 
  summarise(min_ag = min(ag), max_ag = max(ag)) %>% 
  pivot_longer(names_to = 'min_max', values_to = 'ag', -family)
  
genus_family_canag_df <- genus_family_list %>% 
  rbindlist()

## get percent change 

min_max_family %>% 
  left_join(genus_family_canag_df) %>% 
  dplyr::select(family, gg, min_max, mean) %>% 
  pivot_wider(names_from = min_max, values_from = mean) %>% 
  mutate(net_change = min_ag - max_ag, percent_change = 100*(min_ag - max_ag)/min_ag) %>% 
  arrange(percent_change) %>% View()
