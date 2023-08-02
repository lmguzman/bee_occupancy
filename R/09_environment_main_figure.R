#### Script to make Figure 1####

library(ggplot2)
library(purrr)
library(dplyr)
library(sf)
library(cowplot)
library(tidyr)
library(stringr)
library(viridis)

############# MAIN FIGURE 1 ##############

######### summed pesticide load ########

environment <- readRDS("clean_data/data_prepared/environment_counties_1995_2015.rds")

counties <- readRDS("clean_data/sites/sites_counties.RDS") %>% 
  mutate(site = paste0('s_', state_county))

pest_both <- environment$both_mat_area[,"yr2013", drop = FALSE] %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column("site")

pesticide_use_combined <- counties %>% 
  left_join(pest_both) %>% 
  ggplot() +
  geom_sf(aes(fill = yr2013), colour = 'transparent') +
  scale_fill_viridis(name = "Pesticide Use (neonicotinoids \n and pyrethroids)", option = 'viridis') +
  theme_cowplot() +
  theme(legend.position = 'bottom',
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(), 
        axis.line = element_blank(),
        legend.text = element_text(size = 11))


######## percent of the county that is animal pollinated agriculture #######

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


##### honey bees #####

counties <- readRDS("clean_data/sites/sites_counties.RDS")

honey_bee_census <- environment$time_col_mat[,"yr2013", drop = FALSE] %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column("site")

honey_bee_colonies <- counties %>% 
  mutate(site = paste0("s_", state_county)) %>% 
  full_join(honey_bee_census) %>% 
  ggplot() +
  geom_sf(aes(fill = yr2013), colour = 'transparent') +
  theme_cowplot() +
  scale_fill_viridis_c("Number of Honey\n Bee colonies (log)") +
  theme(legend.position = 'bottom',
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(), 
        axis.line = element_blank(),
        legend.text = element_text(size = 11))


##### expected bee richness #####

expected_richness <- readRDS('clean_data/native_expected/expected_richness.rds')

area_counties <- readRDS('clean_data/sites/area_counties.RDS')


wild_bee_richness <- expected_richness %>% 
  left_join(area_counties) %>%
  mutate(area_km2 = area_m_2* 1e-6) %>% 
  mutate(richness_area = units::drop_units(expected_richnness/area_km2)) %>% 
  ggplot() +
  geom_sf(aes(fill = log(richness_area)), color = 'transparent') +
  theme_cowplot() +
  scale_fill_viridis_c("Expected richness per county (log)") +
  theme(legend.position = 'bottom',
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(), 
        axis.line = element_blank(),
        legend.text = element_text(size = 9.5))


Figure_1 <- plot_grid(pesticide_use_combined, animal_pollinated_ag, honey_bee_colonies, wild_bee_richness, 
          labels = c("A.", "B.", "C.", "D."))

ggsave(Figure_1, file = 'plots/Figure1.pdf', width = 10)



