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


get.summ <- function(pars, res.summary) {
  summ <- round(cbind(
    res.summary$summary$statistics[pars,'Mean',drop=FALSE],
    res.summary$summary$quantiles[pars,c('2.5%', '97.5%'),drop=FALSE],
    Rhat=res.summary$psrf$psrf[pars,1]
  ), digits=3)
  colnames(summ)[1] <- 'mean'
  summ
}
species_genus_family <- read.csv("clean_data/native_expected/species_genus_family.csv") 

## set the color palette

n_families <- length(unique(species_genus_family$family))

palette1_named = setNames(object = scales::hue_pal()(n_families), nm = sort(unique(species_genus_family$family)))

##

species_results_ag <- list()
species_results_honey <- list()
species_results_pest <- list()

mcmc_sp_apa <- list()
mcmc_sp_honey <- list()
mcmc_sp_pest <- list()

fam <- c("Andrenidae", "Apidae", "Halictidae", "Megachilidae",  "Colletidae|Melittidae")

for(f in fam){
  
  ## read data
  
  my.data <- readRDS(paste0("clean_data/data_prepared/my_data_env_genus_filtered_trait_agriregion_both_pest_area_county_1995_2015_",f,"_ALLFALSE.rds"))
  
  species_directory <- data.frame(finalName = my.data$sp) %>% 
    mutate(ss = 1:n()) %>% 
    left_join(my.data[[2]])
  
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
    tibble::rownames_to_column("psi.pest") %>% 
    mutate(ss = as.numeric(str_extract(str_extract(psi.pest, "\\[\\d+\\]"), "\\d+"))) %>% 
    left_join(species_directory) %>% 
    left_join(species_genus_family)
  
  ## honey
  
  species_results_honey[[f]] <- summ_honey[str_detect(rownames(summ_honey), 'psi.col\\['),] %>% 
    as.data.frame() %>% 
    tibble::rownames_to_column("psi.pest") %>% 
    mutate(ss = as.numeric(str_extract(str_extract(psi.pest, "\\[\\d+\\]"), "\\d+"))) %>% 
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
    t() %>% 
    as.data.frame() %>% 
    janitor::clean_names() %>% 
    tibble::rownames_to_column("psi.apa") %>% 
    mutate(ss = as.numeric(str_extract(str_extract(psi.apa, "\\[\\d+\\]"), "\\d+"))) %>% 
    left_join(species_directory)
  
  ## honey
  
  mcmc_sp_honey[[f]] <- mcmc_honey[,str_detect(colnames(mcmc_honey), 'psi.col\\[')] %>% 
    t() %>% 
    as.data.frame() %>% 
    janitor::clean_names() %>% 
    tibble::rownames_to_column("psi.honey") %>% 
    mutate(ss = as.numeric(str_extract(str_extract(psi.honey, "\\[\\d+\\]"), "\\d+"))) %>% 
    left_join(species_directory)
  
  ## pesticide
  
  mcmc_sp_pest[[f]] <- mcmc_pest[,str_detect(colnames(mcmc_pest), 'psi.pest1\\[')] %>% 
    t() %>% 
    as.data.frame() %>% 
    janitor::clean_names() %>% 
    tibble::rownames_to_column("psi.pest") %>% 
    mutate(ss = as.numeric(str_extract(str_extract(psi.pest, "\\[\\d+\\]"), "\\d+"))) %>% 
    left_join(species_directory)

}

#### species plots ###

species_ag_plot <- do.call(rbind, species_results_ag) %>% 
  mutate(genus = fct_reorder(genus, desc(genus))) %>% 
  ggplot() +
  geom_point(aes(x = mean, y = genus, colour = family)) +
  #geom_errorbarh(data = species_results, aes(xmin = `2.5%`, xmax = `97.5%`, y = genus, colour = family), height = 0) +
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
  #geom_errorbarh(data = species_results, aes(xmin = `2.5%`, xmax = `97.5%`, y = genus, colour = family), height = 0) +
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
  #geom_errorbarh(data = species_results, aes(xmin = `2.5%`, xmax = `97.5%`, y = genus, colour = family), height = 0) +
  theme_cowplot() +
  geom_vline(xintercept = 0, linetype="dashed", colour = 'grey') +
  ylab('') +
  xlab("Effect of Pesticide Use") +
  theme(legend.position = 'none') +
  scale_colour_manual(values = palette1_named)

species_plot_all <- plot_grid(species_pest_plot, species_ag_plot, species_honey_plot,  labels = c("A.", "B.", "C."), nrow = 1)

ggsave(species_plot_all, file = 'plots/species_averages.pdf', width = 12, height = 15)


######### genus plots all #####

palette1_significance = setNames(object = c('#808080', "#5D3A9B","#E66100"), nm = c('not_sig', "sig_pos", "sig_neg"))

# apa

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

genus_g10_apa <-mcmc_apa_all %>% 
  select(finalName, genus) %>% 
  group_by(genus) %>% 
  count() %>% 
  filter(n > 10)
  
genus_10_plots_apa <- genus_estimates_apa %>% 
  filter(genus %in% genus_g10_apa$genus) %>% 
  ggplot() +
  geom_point(aes(x = mean, y = genus, colour = color_pat)) +
  geom_errorbarh(aes(xmin = x25, xmax = x975, y = genus, colour = color_pat), height = 0) +
  theme_cowplot() +
  geom_vline(xintercept = 0, linetype="dashed", colour = 'grey') +
  ylab('') +
  xlab("Effect of Animal Pollinated Agriculture") +
  theme(legend.position = 'none',
        axis.text.y = element_text(size = 12))+
  scale_colour_manual(values = palette1_significance)


## honey

genus_g10_honey <-mcmc_honey_all %>% 
  select(finalName, genus) %>% 
  group_by(genus) %>% 
  count() %>% 
  filter(n > 10)

genus_10_plots_honey <- genus_estimates_honey %>% 
  filter(genus %in% genus_g10_honey$genus) %>% 
  ggplot() +
  geom_point(aes(x = mean, y = genus, colour = color_pat)) +
  geom_errorbarh(aes(xmin = x25, xmax = x975, y = genus, colour = color_pat), height = 0) +
  theme_cowplot() +
  geom_vline(xintercept = 0, linetype="dashed", colour = 'grey') +
  ylab('') +
  xlab("Effect of Honey Bees") +
  theme(legend.position = 'none',
        axis.text.y = element_text(size = 12))+
  scale_colour_manual(values = palette1_significance)

## pest

genus_g10_pest <-mcmc_pest_all %>% 
  select(finalName, genus) %>% 
  group_by(genus) %>% 
  count() %>% 
  filter(n > 10)

genus_10_plots_pest <- genus_estimates_pest %>% 
  filter(genus %in% genus_g10_pest$genus) %>% 
  ggplot() +
  geom_point(aes(x = mean, y = genus, colour = color_pat)) +
  geom_errorbarh(aes(xmin = x25, xmax = x975, y = genus, colour = color_pat), height = 0) +
  theme_cowplot() +
  geom_vline(xintercept = 0, linetype="dashed", colour = 'grey') +
  ylab('') +
  xlab("Effect of Pesticide Use") +
  theme(legend.position = 'none',
        axis.text.y = element_text(size = 12))+
  scale_colour_manual(values = palette1_significance)

all_genus_g10_plots <- plot_grid(genus_10_plots_pest, genus_10_plots_apa, genus_10_plots_honey,  labels = c("A.", "B.", "C."), nrow = 1)

ggsave(all_genus_g10_plots, file = 'plots/Figure3.pdf', width = 12, height = 8)





