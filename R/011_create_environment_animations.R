library(ggplot2)
library(purrr)
library(dplyr)
library(sf)
library(cowplot)
library(tidyr)
library(stringr)
library(viridis)
library(transformr)
library(gifski)
library(gganimate)

##### agriculture all #####

agriculture <- readRDS(paste0("clean_data/agriculture/agriculture_county.rds"))

counties <- readRDS("clean_data/sites/sites_counties.RDS") %>% 
  mutate(site = paste0("s_", state_county)) 

agriculture_mat <- agriculture %>% 
  mutate(site = paste0("s_", state_county)) %>%
  dplyr::select(-state_county) 

county_agriculture <- counties %>% 
  full_join(agriculture_mat)

  yr <- 2006
  
  ct_yr_ag <- county_agriculture %>% 
    filter(year %in% yr)
  
  ag_plot <- ggplot() +
    geom_sf(data = ct_yr_ag, aes(fill = percent_agriculture), colour = 'black', size = 0.1) +
    theme_cowplot() +
    scale_fill_viridis(name = "Percent of the county \n that is a crop", option = 'viridis', direction = -1)  +
    theme(legend.position = 'bottom',
          axis.title=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(), 
          axis.line = element_blank(),
          legend.text = element_text(size = 11))
  
  ggsave( ag_plot, filename = paste0("plots/environment/agriculture/", yr, ".jpeg"))
  ggsave( ag_plot, filename = paste0("plots/environment/agriculture/", yr, ".pdf"), width = 10)
  


##### pesticide use distribution #####

year_range <- c(1995, 2015)

neonic_raw <- readRDS(paste0("clean_data/pesticide/neonics_US_county.rds")) %>% 
  filter(YEAR >= year_range[1] & YEAR <= year_range[2]) %>% 
  as.data.table()

counties <- readRDS("clean_data/sites/sites_counties.RDS") %>% 
  mutate(site = paste0("s_", state_county)) 


neonic_raw %>% 
  filter(COMPOUND == "IMIDACLOPRID") %>% 
  mutate(site = paste0("s_", state_county))

year_site <- expand.grid(YEAR = year_range[1]:year_range[2], state_county = counties$state_county,
                         COMPOUND = unique(neonic_raw$COMPOUND)) %>% 
  data.table()

setkeyv(year_site, c("YEAR", "state_county", "COMPOUND"))
setkeyv(neonic_raw,  c("YEAR", "state_county", "COMPOUND"))

neonic_all_sites <- neonic_raw[year_site] %>% 
  mutate(pest_site_ld50 = ifelse(is.na(pest_site_ld50), 0, pest_site_ld50))   %>% 
  mutate(site = paste0("s_", state_county))

neonic_sum_log <- neonic_all_sites %>% 
  group_by(site, YEAR) %>% 
  summarise(summed_pesticides = sum(pest_site_ld50, na.rm = TRUE)) %>% 
  mutate(loged_summed_pest = log(summed_pesticides + 1))

county_neonic_all <- counties %>% 
  full_join(neonic_all_sites) %>% 
  mutate(COMPOUND = str_to_title(COMPOUND))

county_neonic_summed <- counties %>% 
  full_join(neonic_sum_log)

### neonic 1995, 2013 and region ###

## increase in neonic between 1995 and 2013 
yr <- c(1995, 2013)

ct_yr_nsum <- county_neonic_summed %>% 
  filter(YEAR %in% yr)

neonic_year <- ggplot() +
  geom_sf(data = ct_yr_nsum, aes(fill = loged_summed_pest), colour = 'grey', size = 0.1) +
  theme_cowplot() +
  scale_fill_viridis(name = "Pesticide Use \n (log scale)", option = 'viridis', direction = -1) +
  theme(legend.position = "bottom", 
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        axis.line = element_blank(), 
        strip.background = element_blank()) +
  facet_wrap(~YEAR, nrow = 2)

ggsave(neonic_year, filename = "plots/environment/pesticide/neonic_region_logged1995.pdf")


## 2013 by region 

yr <- 2013

ct_yr_nsum <- county_neonic_summed %>% 
  filter(YEAR %in% yr)

region_df <- readRDS("clean_data/sites/site_counties_agriregion.rds")%>% 
  mutate(region_collapsed = case_when(region %in% c("Southern Seaboard", "Eastern Uplands",
                                                    "Mississippi Portal") ~ "South East",
                                      region %in% c("Heartland", "Prairie Gateway") ~ "Central",
                                      TRUE ~ region))

counties <- readRDS("clean_data/sites/sites_counties.RDS")

counties_region <- counties %>% 
  left_join(region_df) 

counties_split <- split(counties_region, counties_region$region_collapsed)

regions_combined <- lapply(counties_split, st_union)

neonic_region <- ggplot() +
  geom_sf(data = ct_yr_nsum, aes(fill = loged_summed_pest), colour = 'grey', size = 0.1) +
  theme_cowplot() +
  scale_fill_viridis(name = "Pesticide Use 2013 \n (log scale)", option = 'viridis', direction = -1) +
  theme(legend.position = "bottom", 
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        axis.line = element_blank()) +
  geom_sf(data = regions_combined$`Basin and Range`, fill = "transparent", colour = 'black', size = 0.9) +
  geom_sf(data = regions_combined$Central, fill = "transparent", colour = 'black', size = 0.9) +
  geom_sf(data = regions_combined$`Fruitful Rim`,fill = "transparent",  colour = 'black', size = 0.9) +
  geom_sf(data = regions_combined$`Northern Crescent`, fill = "transparent", colour = 'black', size = 0.9) +
  geom_sf(data = regions_combined$`Northern Great Plains`, fill = "transparent", colour = 'black', size = 0.9) +
  geom_sf(data = regions_combined$`South East`, fill = "transparent", colour = 'black', size = 0.9)

ggsave(neonic_region, filename = "plots/environment/pesticide/neonic_region_logged.jpeg")

ggsave(neonic_region, filename = "plots/environment/pesticide/neonic_region_logged.pdf")


######## plot regions. #########

region_df <- readRDS("clean_data/sites/site_counties_agriregion.rds") %>% 
  mutate(region_collapsed = case_when(region %in% c("Southern Seaboard", "Eastern Uplands",
                                                    "Mississippi Portal") ~ "South East",
                                      region %in% c("Heartland", "Prairie Gateway") ~ "Central",
                                      TRUE ~ region))

counties <- readRDS("clean_data/sites/sites_counties.RDS")

counties_region <- counties %>% 
  left_join(region_df) 
  #left_join(data.frame(region = unique(region_df$region), region_nice = c("West", "South East", "North East", "Center" )))

region_map <- ggplot(data = counties_region) +
  geom_sf(aes(fill = region_collapsed), colour = 'black', size = 0.1) +
  theme_cowplot() +
  scale_fill_discrete(name = "Region")  +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(), 
        axis.line = element_blank())



ggsave(region_map, filename = "plots/environment/regions.jpeg")
ggsave(region_map, filename = "plots/environment/regions.pdf")


###### blacked out regions ####

region_df <- readRDS("clean_data/sites/site_counties_agriregion.rds") %>% 
  mutate(region_collapsed = case_when(region %in% c("Southern Seaboard", "Eastern Uplands",
                                                    "Mississippi Portal") ~ "South East",
                                      region %in% c("Heartland", "Prairie Gateway") ~ "Central",
                                      TRUE ~ region))

counties <- readRDS("clean_data/sites/sites_counties.RDS")

regions <- unique(region_df$region_collapsed)

county_region <- counties %>% 
  left_join(region_df)

US_all <- county_region %>% 
  st_union()

for(r in regions){
  
  region_only <- county_region %>% 
    filter(region_collapsed %in% r) %>% 
    st_union()

 region_outline <- ggplot() +
    geom_sf(data = US_all, fill = 'white') +
    geom_sf(data = region_only, fill = 'black') +
    theme_cowplot()  + 
    theme(legend.position = "bottom", 
          axis.text = element_blank(), 
          axis.ticks = element_blank(), 
          axis.line = element_blank()) 
  
  ggsave(region_outline, filename = paste0("plots/environment/regions_outline/region_",str_replace(r, " ", "_"),".jpeg"))
}


##### Fraction of animal pollinated agriculture for the counties that were modelled ####

files_results <- list.files("clean_data/data_prepared/", full.names = TRUE)

fan_ag_files <- files_results[str_detect(files_results, "my_data_env_genus_filtered_trait_agriregion_both_pest_area")]

agriculture_all <- list()

frac_an_pol_all <- list()


for(f in fan_ag_files){
  
  my.data <- readRDS(f)
  
  region <- str_extract(f, "ALL\\_\\S+\\FALSE")
  
  agriculture_all[[region]] <- my.data[[1]]$agriculture[,"yr2007"]
  
  frac_an_pol_all[[region]] <- my.data[[1]]$fracanimal
  
}

agriculture_all_df <- agriculture_all %>% 
  map_df(~as.data.frame(.x), .id = 'region') %>% 
  tibble::rownames_to_column('site') %>% 
  rename(percent_county_agriculture = .x) %>% 
  mutate(region = str_remove_all(region, "ALL_|FALSE"))


frac_an_pol_all_df <- frac_an_pol_all %>% 
  map_df(~as.data.frame(.x), .id = 'region') %>% 
  tibble::rownames_to_column('site') %>% 
  rename(percent_agriculture_animal_pol = .x) %>% 
  mutate(region = str_remove_all(region, "ALL_|FALSE"))


agriculture_distribution <- agriculture_all_df %>% 
  mutate(region_nice = str_replace_all(region, "\\_", " ")) %>% 
  ggplot(aes(x = region_nice, y = percent_county_agriculture)) +
  geom_boxplot() +
  theme_cowplot() + 
  ylab("Percent of the county \n that is agriculture") +
  xlab("")

apa_distribution <- frac_an_pol_all_df %>% 
  mutate(region_nice = str_replace_all(region, "\\_", " ")) %>% 
  ggplot(aes(x = region_nice, y = percent_agriculture_animal_pol)) +
  geom_boxplot() +
  theme_cowplot() +
  ylab("Percent of the agriculture that \n is animal pollinated agriculture") +
  xlab("") 

ag_distribution_region <- plot_grid(agriculture_distribution, apa_distribution, nrow = 2, labels = c("A.", "B."))

ggsave(ag_distribution_region, file = 'plots/ag_distribution.pdf', height = 9)









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
  geom_sf(aes(fill = yr2013)) +
  scale_fill_viridis(name = "Pesticide Use (neonicotinoids \n and pyrethroids)", option = 'viridis') +
  theme_cowplot() +
  theme(legend.position = 'bottom',
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(), 
        axis.line = element_blank(),
        legend.text = element_text(size = 11))

ggsave(pesticide_use_combined, filename = "plots/environment/pesticide_use.pdf", width = 10)


######## percent of the county that is animal pollinated agriculture #######

region_agriculture_all <- readRDS("clean_data/agriculture/crops_county_animal.rds")

Total_county_area <- region_agriculture_all %>% 
  group_by(site) %>% 
  summarise(total_area = sum(Freq))

Total_animal_pollinated <- region_agriculture_all %>% 
  filter(cover_type == 'Crop') %>% 
  filter(non_animal_pollinated %in% c("FALSE", "HALF")) %>% 
  group_by(site) %>% 
  summarise(total_ani_pollinated = sum(Freq))

frac_animal_pollinated_all <- Total_county_area %>% 
  left_join(Total_animal_pollinated) %>% 
  mutate(county_animal_pol = total_ani_pollinated/total_area) 

counties <- readRDS("clean_data/sites/sites_counties.RDS")

frac_animal_pollinated_all_sh <- counties %>% 
  mutate(site = paste0("s_", state_county)) %>% 
  left_join(frac_animal_pollinated_all)

animal_pollinated_ag <- ggplot() +
  geom_sf(data = frac_animal_pollinated_all_sh, aes(fill = log(county_animal_pol))) +
  theme_cowplot() +
  scale_fill_viridis(name = "Percent of county \n that is animal pollinated \n agriculture (log)", option = 'viridis') +
  theme(legend.position = 'bottom',
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(), 
        axis.line = element_blank(),
        legend.text = element_text(size = 11))

ggsave(animal_pollinated_ag, file = 'plots/environment/animal_pol_ag.pdf', width = 10)


##### honey bees #####

counties <- readRDS("clean_data/sites/sites_counties.RDS")

honey_bee_census <- readRDS("clean_data/honey_bees/colonies_time.rds") %>% 
  filter(Year == 2012)

honey_bee_colonies <- counties %>% 
  full_join(honey_bee_census) %>% 
  mutate(Value = ifelse(is.na(Value), 0, Value)) %>% 
  ggplot() +
  geom_sf(aes(fill = log(Value + 1))) +
  theme_cowplot() +
  scale_fill_viridis_c("Number of Honey\n Bee colonies (log)") +
  theme(legend.position = 'bottom',
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(), 
        axis.line = element_blank(),
        legend.text = element_text(size = 11))

ggsave(honey_bee_colonies, file = 'plots/environment/honey_bee_colonies.pdf', width = 10)


##### expected bee richness #####

expected_richness <- readRDS('clean_data/native_expected/expected_richness.rds')

wild_bee_richness <- ggplot() +
  geom_sf(data = expected_richness, aes(fill = expected_richnness)) +
  theme_cowplot() +
  scale_fill_viridis_c("Expected richness per county") +
  theme(legend.position = 'bottom',
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(), 
        axis.line = element_blank(),
        legend.text = element_text(size = 9.5))

ggsave(wild_bee_richness, file = 'plots/environment/wild_bee_richness.pdf', width = 10)

