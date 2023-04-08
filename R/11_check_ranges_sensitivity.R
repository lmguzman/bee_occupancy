library(dplyr)
library(ggplot2)
library(purrr)
library(cowplot)

## load in data##

ranges <- readRDS("clean_data/ranges/ranges_counties.rds")

regions <- readRDS("clean_data/sites/site_counties_region.rds")

area <- readRDS("clean_data/sites/area_counties.RDS")

range_df <- ranges %>% 
  map_df(~as.data.frame(.x), .id = 'species') %>% 
  rename(state_county = `.x`)

## number of species per region

species_by_region <- range_df %>% 
  left_join(regions) %>% 
  select(region, species) %>% 
  unique()
  
n_species_by_region <- species_by_region %>% 
  group_by(region) %>% 
  summarise(n = n())

## number of counties per region 

n_county_region <- regions %>% 
  group_by(region) %>% 
  summarise(n = n())

### distribution of counties per region -- how many counties are in each species range per region ###

n_counties_species_region <- range_df %>% 
  left_join(regions) %>% 
  group_by(species, region) %>% 
  unique() %>% 
  summarise(n = n())

n_counties_species_region %>% 
  ggplot(aes(x = n)) +
  facet_wrap(~region) +
  geom_histogram() +
  theme_cowplot() +
  xlab("Number of counties per species") +
  theme(strip.background = element_blank()) +
  geom_text(data = n_species_by_region, aes(y = 200, x = 700, label = paste("N species =", n))) +
  geom_text(data = n_county_region, aes(y = 150, x = 700, label = paste("N counties =", n)))

## area of species ranges per region 

area_species_region <- range_df %>% 
  left_join(regions) %>% 
  left_join(area) %>% 
  group_by(species, region) %>% 
  unique() %>% 
  summarise(area_range = sum(area_m_2) * 1e-6)


area_species_region %>% 
  ggplot(aes(x = area_range)) +
  facet_wrap(~region) +
  geom_histogram() +
  theme_cowplot() +
  xlab("Area of counties per species Km2") +
  theme(strip.background = element_blank()) +
  geom_text(data = n_species_by_region, aes(y = 100, x = 1.5e+6, label = paste("N species =", n))) +
  geom_text(data = n_county_region, aes(y = 80, x = 1.5e+6, label = paste("N counties =", n)))



#### correlation between counties with pesticide and counties with bees
library(data.table)
library(stringr)

regions <- readRDS("clean_data/sites/site_counties_agriregion.rds")

region_df <- regions %>% 
  mutate(region_collapsed = case_when(region %in% c("Southern Seaboard", "Eastern Uplands",
                                                    "Mississippi Portal") ~ "South East",
                                      region %in% c("Heartland", "Prairie Gateway") ~ "Central",
                                      TRUE ~ region))

observations_raw <- readRDS(paste0("clean_data/observations/observations_counties.rds"))

observations_raw$site <- paste0("s_", observations_raw$state_county)

year_range <- c(1995, 2015)

environmental_data <- readRDS(paste0("clean_data/data_prepared/environment_counties_", paste0(year_range, collapse = "_"),".rds"))

has_bees_pest_list <- list()

has_bees_pest_list_genus <- list()

for(r in unique(region_df$region_collapsed)){
  
  chosen_state_county <- paste0("s_", filter(region_df, region_collapsed == r)$state_county)
  
  environmental_data$site_id2 <- environmental_data$site_id[environmental_data$site_id %in% chosen_state_county]
  
  observations <- observations_raw[site %in% environmental_data$site_id2 & year >=  year_range[1] & year <=  year_range[2]]
  
  pesticide <- environmental_data$neonic_mat[environmental_data$site_id2,]
  
  any_year_pest <- rowSums(pesticide != min(pesticide))
  
  any_year_pest[any_year_pest > 0] <- 1
  
  has_pest_bees <- data.frame(has_pest = any_year_pest) %>% 
    tibble::rownames_to_column("site") %>% 
    left_join(data_frame(site = unique(observations$site), has_bees = 1)) %>% 
    mutate(has_bees = ifelse(is.na(has_bees), 0, has_bees))
  
  has_bees_pest_list[[r]] <- table(has_pest = has_pest_bees$has_pest, has_bees =  has_pest_bees$has_bees)
  
  
  observations$genus <- str_extract(observations$finalName, "[A-Z][a-z]*")
  
  ### remove anything that is the only species in that genus
  
  nsp_genus <- observations %>% 
    select(finalName, genus) %>% 
    unique() %>% 
    group_by(genus) %>% 
    summarise(n_sp = n())
  
  genus_moreone <- nsp_genus %>% 
    filter(n_sp > 1)
  
  observations_clean_vis_moreone <- observations %>% 
    filter(genus %in% genus_moreone$genus)
  
  has_pest_bees_genus <- data.frame(has_pest = any_year_pest) %>% 
    tibble::rownames_to_column("site") %>% 
    left_join(data_frame(site = unique(observations_clean_vis_moreone$site), has_bees = 1)) %>% 
    mutate(has_bees = ifelse(is.na(has_bees), 0, has_bees))
  
  has_bees_pest_list_genus[[r]] <- table(has_pest_bees_genus$has_pest, has_pest_bees_genus$has_bees)
  
}



