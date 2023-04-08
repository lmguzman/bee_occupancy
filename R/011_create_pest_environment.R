library(dplyr)
library(data.table)
library(sf)
library(ggplot2)
library(cowplot)
library(stringr)
library(viridis)
library(tidyr)

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



## correlation between both pesticides and percent agriculture ##
## check for every region##

environmental_data <- readRDS(paste0("clean_data/data_prepared/environment_counties_1995_2015.rds"))



both_df <- environmental_data$both_mat %>% 
  data.frame() %>% 
  tibble::rownames_to_column("site") %>% 
  pivot_longer(names_to = 'year', values_to = 'neonic', -site)

ag_df <- environmental_data$ag_mat %>% 
  data.frame() %>% 
  tibble::rownames_to_column("site") %>% 
  pivot_longer(names_to = 'year', values_to = 'pyr', -site)

both_ag <- both_df %>% 
  left_join(ag_df) 

correlations_df <- list()

for(yr in sort(unique(both_ag$year))){
  
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