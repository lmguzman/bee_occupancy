##### Script to create environmental figures S3, S4, S5, S6, S7, S8, and S9 #######

library(dplyr)
library(data.table)
library(sf)
library(ggplot2)
library(cowplot)
library(stringr)
library(viridis)
library(tidyr)
library(broom)

## load environmental data and counties 

environmental_data <- readRDS(paste0("clean_data/data_prepared/environment_counties_1995_2015.rds"))

counties <- readRDS("clean_data/sites/sites_counties.RDS")

######## Figure S3 ######

## plotting neonics on 1995 and 2013 ##

neonic_df <- environmental_data$neonic_mat %>% 
  data.frame() %>% 
  tibble::rownames_to_column("site")

county_neonic <- counties %>% 
  mutate(site = paste0('s_', state_county)) %>% 
  left_join(neonic_df)

neonic_1995 <- ggplot() +
  geom_sf(data = county_neonic, aes(fill = yr1995), colour = 'transparent', size = 0.01) +
  theme_cowplot() +
  scale_fill_viridis(name = "Neonicotinoid 1995", option = 'viridis', direction = 1) +
  theme(legend.position = "bottom", 
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        axis.line = element_blank())

neonic_2013 <- ggplot() +
  geom_sf(data = county_neonic, aes(fill = yr2013), colour = 'transparent', size = 0.01) +
  theme_cowplot() +
  scale_fill_viridis(name = "Neonicotinoid 2013", option = 'viridis', direction = 1) +
  theme(legend.position = "bottom", 
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        axis.line = element_blank())

## plotting pyrethroids in 1995 and 2013 ##

pyr_df <- environmental_data$pyr_mat %>% 
  data.frame() %>% 
  tibble::rownames_to_column("site")

county_pyr <- counties %>% 
  mutate(site = paste0('s_', state_county)) %>% 
  left_join(pyr_df)

pyr_1995 <- ggplot() +
  geom_sf(data = county_pyr, aes(fill = yr1995), colour = 'transparent', size = 0.01) +
  theme_cowplot() +
  scale_fill_viridis(name = "Pyrethroid 1995", option = 'viridis', direction = 1) +
  theme(legend.position = "bottom", 
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        axis.line = element_blank())

pyr_2013 <- ggplot() +
  geom_sf(data = county_pyr, aes(fill = yr2013), colour = 'transparent', size = 0.01) +
  theme_cowplot() +
  scale_fill_viridis(name = "Pyrethroid 2013", option = 'viridis', direction = 1) +
  theme(legend.position = "bottom", 
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        axis.line = element_blank())



total_pesticide_use <- plot_grid(neonic_1995, neonic_2013, pyr_1995, pyr_2013, labels = c("A.", "B.", "C.", "D."))

ggsave(total_pesticide_use, filename = "plots/environment/pesticide_increase_all.pdf")


######## Figures S4 and S5 ########

##### Each pesticide we use and the LD50 plot #####

## neonic ##

year_range <- c(1995, 2015)

## load raw pesticide data 

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

### do the plot for each compound

all_compounds_neonic_2013 <- ggplot() +
  geom_sf(data = all_us, colour = 'black', size = 2) +
  geom_sf(data = county_neonic_comp, aes(fill = log(pest_site)), colour = "transparent", size = 0.1) +
  theme_cowplot() +
  facet_wrap(~compound) +
  scale_fill_viridis(name = "Kg of compound applied 2013 (log)", option = 'viridis', direction = 1, na.value="white") +
  theme(legend.position = "bottom", 
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        axis.line = element_blank(), 
        strip.background = element_blank())

ggsave(all_compounds_neonic_2013, filename = "plots/environment/all_neonics.pdf")



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

## do the plot for each compound

all_compounds_pyr_2013 <- ggplot() +
  geom_sf(data = all_us, colour = 'black', size = 2) +
  geom_sf(data = county_pyr_comp, aes(fill = log(pest_site)), colour = 'transparent', size = 0.1) +
  theme_cowplot() +
  facet_wrap(~compound) +
  scale_fill_viridis(name = "Kg of compound applied 2013 (log)", option = 'viridis', direction = 1, na.value="white") +
  theme(legend.position = "bottom", 
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        axis.line = element_blank(), 
        strip.background = element_blank())

ggsave(all_compounds_pyr_2013, filename = "plots/environment/all_pyrethroids.pdf")

####### Information for Table S1 ######

### LD 50 of all compounds ##

pyr_raw %>% 
  select(COMPOUND, mean_ld50) %>% unique() %>% 
  mutate(COMPOUND = str_to_title(COMPOUND))

neonic_raw %>% 
  select(COMPOUND, mean_ld50) %>% unique() %>% 
  mutate(COMPOUND = str_to_title(COMPOUND))


##### Figure S6 ########

environment <- readRDS("clean_data/data_prepared/environment_counties_1995_2015.rds")

counties <- readRDS("clean_data/sites/sites_counties.RDS") %>% 
  mutate(site = paste0('s_', state_county))

## plain agriculture 

agriculture_all_sh <- counties %>% 
  mutate(site = paste0("s_", state_county)) %>% 
  left_join(data.frame(ag = environment$ag_mat[, "yr2007"]) %>% 
              tibble::rownames_to_column("site"))

main_ag <- ggplot() +
  geom_sf(data = agriculture_all_sh, aes(fill = ag), colour = 'transparent') +
  theme_cowplot() +
  scale_fill_viridis(name = "Percent of county \n that is \n agriculture (log)", option = 'viridis') +
  theme(legend.position = 'bottom',
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(), 
        axis.line = element_blank(),
        legend.text = element_text(size = 11))

### fraction that is animal pollinated 

frac_animal_pollinated_all_sh <- counties %>% 
  mutate(site = paste0("s_", state_county)) %>% 
  left_join(data.frame(county_animal_pol = environment$county_fan_mat) %>% 
              tibble::rownames_to_column("site"))

animal_pollinated_ag <- ggplot() +
  geom_sf(data = frac_animal_pollinated_all_sh, aes(fill = county_animal_pol), colour = 'transparent') +
  theme_cowplot() +
  scale_fill_viridis(name = "Percent of county \n that is animal pollinated \n agriculture (log)", option = 'viridis') +
  theme(legend.position = 'bottom',
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(), 
        axis.line = element_blank(),
        legend.text = element_text(size = 11))


### fraction that uses managed bees

frac_animal_pollinated_all_sh <- counties %>% 
  mutate(site = paste0("s_", state_county)) %>% 
  left_join(data.frame(county_animal_pol = environment$county_fan_mat_mb) %>% 
              tibble::rownames_to_column("site"))

animal_pollinated_ag_mb <- ggplot() +
  geom_sf(data = frac_animal_pollinated_all_sh, aes(fill = county_animal_pol), colour = 'transparent') +
  theme_cowplot() +
  scale_fill_viridis(name = "Percent of county \n that is agriculture that \n uses managed pollinators (log)", option = 'viridis') +
  theme(legend.position = 'bottom',
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(), 
        axis.line = element_blank(),
        legend.text = element_text(size = 11))


#### fraction that is attractive to bumble bees and solitary bees

frac_animal_pollinated_all_sh <- counties %>% 
  mutate(site = paste0("s_", state_county)) %>% 
  left_join(data.frame(county_animal_pol = environment$county_fan_mat_abs) %>% 
              tibble::rownames_to_column("site"))

animal_pollinated_ag_abs <- ggplot() +
  geom_sf(data = frac_animal_pollinated_all_sh, aes(fill = county_animal_pol), colour = 'transparent') +
  theme_cowplot() +
  scale_fill_viridis(name = "Percent of county \n that is agriculture that \n attracts bumble bees and solitary bees (log)", option = 'viridis') +
  theme(legend.position = 'bottom',
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(), 
        axis.line = element_blank(),
        legend.text = element_text(size = 11))


agriculture_comparison <- plot_grid(main_ag, animal_pollinated_ag, animal_pollinated_ag_mb, animal_pollinated_ag_abs, 
          labels = c("A.", "B.", "C.", "D."))

ggsave(agriculture_comparison, file = "plots/environment/agriculture_comparison.pdf", width = 10)


####### Figure S7 #######

### correlation between animal pollinated agriculture and pesticide use ###

library(broom)

env_all <- readRDS("clean_data/data_prepared/environment_counties_1995_2015.rds")

fam <- c("Andrenidae", "Apidae", "Halictidae", 
         "Megachilidae", "Colletidae|Melittidae")

canag_pest_plot_all <- list()

canag_correlation_all <- list()

for(f in fam){
  
  # load data
  
  my.data <- readRDS(paste0("clean_data/data_prepared/my_data_env_genus_filtered_trait_agriregion_both_pest_area_county_1995_2015_",f,"_ALLFALSE.rds"))
  
  ## calculate correlation
  
  canag_correlation_all[[f]] <- apply(my.data[[1]]$pesticidearea, 2, FUN = function(x) tidy(cor.test(my.data[[1]]$countanimal, x))) %>% 
    map_df(~as.data.frame(.x), .id = 'year') %>% 
    mutate(family = f)
  
  ## plot
  
  canag_pest_plot_all[[f]] <- data.frame(canag = my.data[[1]]$countanimal, my.data[[1]]$pesticidearea) %>% 
    pivot_longer(names_to = "year", values_to = "pest", -canag) %>% 
    ggplot(aes(x = canag, y = pest, colour = str_remove(year, "yr"))) + 
    geom_point() +
    theme_cowplot() +
    ggtitle(str_replace(f, "_", " ")) +
    xlab("Proportion of the county that is \n animal pollinated agriculture")+
    ylab("Pesticide Use") +
    scale_color_discrete(name = "Year") +
    theme(axis.text = element_text(size = 2))

}

canag_correlation_all %>% 
  map_df(~as.data.frame(.x)) %>% 
  arrange(estimate)

correlations_all <- plot_grid(canag_pest_plot_all[[1]], canag_pest_plot_all[[2]], canag_pest_plot_all[[3]],
                              canag_pest_plot_all[[4]], canag_pest_plot_all[[5]])

ggsave(correlations_all, file = "plots/environment/correlation_pest_APA.pdf", width = 10)





###### Figure S8 ##########

expected_richness <- readRDS('clean_data/native_expected/expected_richness.rds')

area_counties <- readRDS('clean_data/sites/area_counties.RDS')

wild_bee_richness_area <- expected_richness %>% 
  left_join(area_counties) %>%
  mutate(area_km2 = area_m_2* 1e-6) %>% 
  mutate(richness_area = units::drop_units(expected_richnness/area_km2)) %>% 
  ggplot() +
  geom_sf(aes(fill = log(richness_area)), color = 'transparent') +
  theme_cowplot() +
  scale_fill_viridis_c("Expected richness divided \n by county area (log)") +
  theme(legend.position = 'bottom',
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(), 
        axis.line = element_blank(),
        legend.text = element_text(size = 9.5))


wild_bee_richness_raw <- expected_richness %>% 
  ggplot() +
  geom_sf(aes(fill = expected_richnness), color = 'transparent') +
  theme_cowplot() +
  scale_fill_viridis_c("Expected richness") +
  theme(legend.position = 'bottom',
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(), 
        axis.line = element_blank(),
        legend.text = element_text(size = 9.5))

expected_bees_transform <- plot_grid(wild_bee_richness_area, wild_bee_richness_raw, 
                      labels = c("A.", "B."), ncol =1)

ggsave(expected_bees_transform, file = 'plots/expected_bees_transform.pdf')





######## Figure S9 #######

### number of sites and sites modeled for each family ###

fam <- c("Andrenidae", "Apidae", "Halictidae", 
         "Megachilidae", "Colletidae|Melittidae")

counties <- readRDS("clean_data/sites/sites_counties.RDS") %>% 
  mutate(site = paste0("s_", state_county))

counties_modelled_plot <- list()

for(f in fam){
  
  my.data <- readRDS(paste0("clean_data/data_prepared/my_data_env_genus_filtered_trait_agriregion_both_pest_area_county_1995_2015_",f,"_ALLFALSE.rds"))
  
  ## check number of sites and species 
  print(f)
  print(my.data[[1]]$nsite)
  print(my.data[[1]]$nsp)
  
  # figure out which counties were modeled
  
  counties_used <- counties %>%
    left_join(data.frame(site = my.data$site, present = 1)) %>% 
    mutate(present = ifelse(is.na(present), 0, present))
  
  # plot the counties
  
  counties_modelled_plot[[f]] <- ggplot() +
    geom_sf(data = counties_used, aes(fill = factor(present)), colour = 'transparent') +
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

