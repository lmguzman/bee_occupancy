library(dplyr)
library(data.table)
library(sf)
library(ggplot2)
library(cowplot)
library(stringr)
library(viridis)
library(tidyr)
library(broom)

####### create plot based on matrix we put into the model ####

environmental_data <- readRDS(paste0("clean_data/data_prepared/environment_counties_1995_2015.rds"))

counties <- readRDS("clean_data/sites/sites_counties.RDS")

## neonic ##

neonic_df <- environmental_data$neonic_mat %>% 
  data.frame() %>% 
  tibble::rownames_to_column("site")

county_neonic <- counties %>% 
  mutate(site = paste0('s_', state_county)) %>% 
  left_join(neonic_df)

neonic_1995 <- ggplot() +
  geom_sf(data = county_neonic, aes(fill = yr1995), colour = 'grey', size = 0.1) +
  theme_cowplot() +
  scale_fill_viridis(name = "Neonicotinoid 1995", option = 'viridis', direction = -1) +
  theme(legend.position = "bottom", 
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        axis.line = element_blank())

neonic_2013 <- ggplot() +
  geom_sf(data = county_neonic, aes(fill = yr2013), colour = 'grey', size = 0.1) +
  theme_cowplot() +
  scale_fill_viridis(name = "Neonicotinoid 2013", option = 'viridis', direction = -1) +
  theme(legend.position = "bottom", 
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        axis.line = element_blank())


## pyrethroid ##

pyr_df <- environmental_data$pyr_mat %>% 
  data.frame() %>% 
  tibble::rownames_to_column("site")

county_pyr <- counties %>% 
  mutate(site = paste0('s_', state_county)) %>% 
  left_join(pyr_df)

pyr_1995 <- ggplot() +
  geom_sf(data = county_pyr, aes(fill = yr1995), colour = 'grey', size = 0.1) +
  theme_cowplot() +
  scale_fill_viridis(name = "Pyrethroid 1995", option = 'viridis', direction = -1) +
  theme(legend.position = "bottom", 
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        axis.line = element_blank())

pyr_2013 <- ggplot() +
  geom_sf(data = county_pyr, aes(fill = yr2013), colour = 'grey', size = 0.1) +
  theme_cowplot() +
  scale_fill_viridis(name = "Pyrethroid 2013", option = 'viridis', direction = -1) +
  theme(legend.position = "bottom", 
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        axis.line = element_blank())



total_pesticide_use <- plot_grid(neonic_1995, neonic_2013, pyr_1995, pyr_2013, labels = c("A.", "B.", "C.", "D."))

ggsave(total_pesticide_use, filename = "plots/environment/pesticide/all.jpeg")

ggsave(total_pesticide_use, filename = "plots/environment/pesticide/all.pdf")



##### Each pesticide we use and the LD50 plot #####

## neonic ##

year_range <- c(1995, 2015)

neonic_raw <- readRDS(paste0("clean_data/pesticide/neonics_US_county.rds")) %>% 
  filter(YEAR >= year_range[1] & YEAR <= year_range[2]) %>% 
  as.data.table()

counties <- readRDS("clean_data/sites/sites_counties.RDS") %>% 
  mutate(site = paste0("s_", state_county)) 

all_us <- st_union(counties)

## fill in gaps for sites where no pesticide use detected 

year_site <- expand.grid(YEAR = year_range[1]:year_range[2], state_county = counties$state_county,
                         COMPOUND = unique(neonic_raw$COMPOUND)) %>% 
  data.table()

setkeyv(year_site, c("YEAR", "state_county", "COMPOUND"))
setkeyv(neonic_raw,  c("YEAR", "state_county", "COMPOUND"))

neonic_all_sites <- neonic_raw[year_site] %>% 
  mutate(pest_site_ld50 = ifelse(is.na(pest_site_ld50), 0, pest_site_ld50))   %>% 
  mutate(pest_site = ifelse(is.na(pest_site), 0, pest_site))   %>% 
  mutate(site = paste0("s_", state_county)) %>% 
  filter(YEAR == 2013)

county_neonic_comp <- counties %>% 
  mutate(site = paste0('s_', state_county)) %>% 
  left_join(neonic_all_sites) %>% 
  mutate(compound = str_to_title(COMPOUND))

all_compounds_neonic_2013 <- ggplot() +
  geom_sf(data = all_us, colour = 'black', size = 2) +
  geom_sf(data = county_neonic_comp, aes(fill = log(pest_site)), colour = "white", size = 0.1) +
  theme_cowplot() +
  facet_wrap(~compound) +
  scale_fill_viridis(name = "Kg of compound applied 2013 (log)", option = 'viridis', direction = -1, na.value="white") +
  theme(legend.position = "bottom", 
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        axis.line = element_blank(), 
        strip.background = element_blank())

ggsave(all_compounds_neonic_2013, filename = "plots/environment/pesticide/all_neonics.jpeg")
ggsave(all_compounds_neonic_2013, filename = "plots/environment/pesticide/all_neonics.pdf")



## pyrethroids ##

year_range <- c(1995, 2015)

pyr_raw <- readRDS(paste0("clean_data/pesticide/pyr_US_county.rds")) %>% 
  filter(YEAR >= year_range[1] & YEAR <= year_range[2]) %>% 
  as.data.table()

counties <- readRDS("clean_data/sites/sites_counties.RDS") %>% 
  mutate(site = paste0("s_", state_county)) 


## fill in gaps for sites where no pesticide use detected 

year_site <- expand.grid(YEAR = year_range[1]:year_range[2], state_county = counties$state_county,
                         COMPOUND = unique(pyr_raw$COMPOUND)) %>% 
  data.table()

setkeyv(year_site, c("YEAR", "state_county", "COMPOUND"))
setkeyv(pyr_raw,  c("YEAR", "state_county", "COMPOUND"))

pyr_all_sites <- pyr_raw[year_site] %>% 
  mutate(pest_site_ld50 = ifelse(is.na(pest_site_ld50), 0, pest_site_ld50))   %>% 
  mutate(pest_site = ifelse(is.na(pest_site), 0, pest_site))   %>% 
  mutate(site = paste0("s_", state_county)) %>% 
  filter(YEAR == 2013)

county_pyr_comp <- counties %>% 
  mutate(site = paste0('s_', state_county)) %>% 
  left_join(pyr_all_sites) %>% 
  mutate(compound = str_to_title(COMPOUND))

all_compounds_pyr_2013 <- ggplot() +
  geom_sf(data = all_us, colour = 'black', size = 2) +
  geom_sf(data = county_pyr_comp, aes(fill = log(pest_site)), colour = 'white', size = 0.1) +
  theme_cowplot() +
  facet_wrap(~compound) +
  scale_fill_viridis(name = "Kg of compound applied 2013 (log)", option = 'viridis', direction = -1, na.value="white") +
  theme(legend.position = "bottom", 
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        axis.line = element_blank(), 
        strip.background = element_blank())

ggsave(all_compounds_pyr_2013, filename = "plots/environment/pesticide/all_pyrethroids.jpeg")
ggsave(all_compounds_pyr_2013, filename = "plots/environment/pesticide/all_pyrethroids.pdf")


### LD 50 of all compounds ##

pyr_raw %>% 
  select(COMPOUND, mean_ld50) %>% unique() %>% 
  mutate(COMPOUND = str_to_title(COMPOUND))

neonic_raw %>% 
  select(COMPOUND, mean_ld50) %>% unique() %>% 
  mutate(COMPOUND = str_to_title(COMPOUND))

### correlation between pyrethoids and neonics ##

environmental_data <- readRDS(paste0("clean_data/data_prepared/environment_counties_1995_2015.rds"))

neonic_df <- environmental_data$neonic_mat %>% 
  data.frame() %>% 
  tibble::rownames_to_column("site") %>% 
  pivot_longer(names_to = 'year', values_to = 'neonic', -site)

pyr_df <- environmental_data$pyr_mat %>% 
  data.frame() %>% 
  tibble::rownames_to_column("site") %>% 
  pivot_longer(names_to = 'year', values_to = 'pyr', -site)

neonic_pyr <- neonic_df %>% 
  left_join(pyr_df) 

correlations_df <- list()

for(yr in sort(unique(neonic_pyr$year))){
  
  neonic_pyr_yr <- neonic_pyr %>% 
    filter(year == yr)
  
  cor_tested <- cor.test(neonic_pyr_yr$neonic, neonic_pyr_yr$pyr)
  
  correlations_df[[yr]] <- data.frame(yr = yr, cor.val = cor_tested$estimate, p_val = cor_tested$p.value)
  
}
## correlation between pyrethoids and neonics ##
correlations_df %>% 
  map_df(~as.data.frame(.x)) %>% 
  mutate(significant = ifelse(p_val < 0.05, TRUE, FALSE)) %>% 
  arrange(cor.val)

## for every year the correlation between pyrethroids and neonics is significant ##
## the correlation is positive ##
## the correlation is getting stronger over time ##


#### check diversity of crops in each region using the crop data layer ####

## read in crop data layer

### number of species per region  per family 

bee_data <- readRDS(file = paste0("clean_data/observations/observations_counties.rds"))

genus_family <- bee_data[,.(genus, family)] %>% unique() %>% as.data.frame() %>% 
  filter(genus != "") %>% 
  filter(!(genus == "Ashmeadiella" & family == "Apidae")) %>% 
  filter(!(genus == "Svastra" & family == "Megachilidae")) 

species_region <- number_species_family %>% 
  map(~as.data.frame(.x)) %>% 
  map(~left_join(.x, genus_family)) %>% 
  map_df(~as.data.frame(.x), .id = region) %>% 
  mutate(family = ifelse(is.na(family), "Apidae", family))
#
species_family <- species_region %>% 
  dplyr::select(family, finalName) %>% unique() %>% 
  count(family)

species_family$n  %>% sum()

species_genus_family <- species_region %>% 
  dplyr::select(finalName, genus, family) %>% 
  unique()

write.csv(species_genus_family, "clean_data/native_expected/species_genus_family.csv", row.names = FALSE)



#### at the family level ####

### number of sites and sites modelled for each familuy ###

fam <- c("Andrenidae", "Apidae", "Halictidae", 
         "Megachilidae", "Colletidae|Melittidae")

counties <- readRDS("clean_data/sites/sites_counties.RDS") %>% 
  mutate(site = paste0("s_", state_county))

counties_modelled_plot <- list()

for(f in fam){
  
  my.data <- readRDS(paste0("clean_data/data_prepared/my_data_env_genus_filtered_trait_agriregion_both_pest_area_county_1995_2015_",f,"_ALLFALSE.rds"))

  print(f)
  print(my.data[[1]]$nsite)
  print(my.data[[1]]$nsp)
  
  counties_used <- counties %>%
    left_join(data.frame(site = my.data$site, present = 1)) %>% 
    mutate(present = ifelse(is.na(present), 0, present))
  
  counties_modelled_plot[[f]] <- ggplot() +
    geom_sf(data = counties_used, aes(fill = factor(present)), colour = '#F2F3F4') +
    theme_cowplot() +
    scale_fill_manual(values = c('white', 'black')) +
    theme(legend.position = "none", 
          axis.text = element_blank(), 
          axis.ticks = element_blank(), 
          axis.line = element_blank(), 
          strip.background = element_blank()) +
    ggtitle(f)
  

}


counties_modelled_all <- plot_grid(counties_modelled_plot[[1]], counties_modelled_plot[[2]], counties_modelled_plot[[3]], counties_modelled_plot[[4]], counties_modelled_plot[[5]], ncol = 2)

ggsave(counties_modelled_all, file = 'plots/counties_modelled.pdf')



### correlation between canag and pest ###

library(broom)

env_all <- readRDS("clean_data/data_prepared/environment_counties_1995_2015.rds")

fam <- c("Andrenidae", "Apidae", "Halictidae", 
         "Megachilidae", "Colletidae|Melittidae")

canag_pest_plot_all <- list()

canag_correlation_all <- list()

for(f in fam){
  
  my.data <- readRDS(paste0("clean_data/data_prepared/my_data_env_genus_filtered_trait_agriregion_both_pest_area_county_1995_2015_",f,"_ALLFALSE.rds"))
  
  canag_correlation_all[[f]] <- apply(my.data[[1]]$pesticide1, 2, FUN = function(x) tidy(cor.test(my.data[[1]]$countanimal, x))) %>% 
    map_df(~as.data.frame(.x), .id = 'year') %>% 
    mutate(family = f)
  
  canag_pest_plot_all[[f]] <- data.frame(canag = my.data[[1]]$countanimal, my.data[[1]]$pesticide1) %>% 
    pivot_longer(names_to = "year", values_to = "pest", -canag) %>% 
    ggplot(aes(x = canag, y = pest, colour = str_remove(year, "yr"))) + 
    geom_point() +
    theme_cowplot() +
    ggtitle(str_replace(f, "_", " ")) +
    xlab("Proportion of the county that is \n animal pollinated agriculture")+
    ylab("Pesticide Use") +
    scale_color_discrete(name = "Year")

  
}


canag_correlation_all %>% 
  map_df(~as.data.frame(.x)) %>% 
  ggplot(aes(x = year, y = estimate, colour = family, group = family)) +
  geom_line() +
  ylab("correlation between canag and pest")

correlations_all <- plot_grid(canag_pest_plot_all[[1]], canag_pest_plot_all[[2]], canag_pest_plot_all[[3]],
                              canag_pest_plot_all[[4]], canag_pest_plot_all[[5]])

ggsave(correlations_all, file = "plots/int_data_plots/correlation_pest_APA.pdf")



canag_correlation_all[[5]] %>% summary()
