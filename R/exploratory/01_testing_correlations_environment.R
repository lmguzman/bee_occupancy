#### testing correlations between environmental variables ###


## climate 
library(dplyr)
library(ggplot2)
library(purrr)

climate <- readRDS("clean_data/climate/climate_counties.rds")
year_range <- c(1995, 2015)

climate <- climate %>% 
  map_df(~as.data.frame(.x)) %>% 
  filter(year >= year_range[1] & year <= year_range[2])

temp_scaled <- climate %>%
  mutate(site = paste0("s_", state_county)) %>% 
  filter(variable == 'tmax' & month %in% c(7,8)) %>% 
  group_by(site, year) %>% 
  summarise(max_t_year = max(values, na.rm = TRUE)/10) %>% 
  mutate(year = paste0("yr", year)) %>%
  filter(!is.infinite(max_t_year), !is.na(max_t_year)) %>% 
  ungroup() %>% 
  mutate(scaled_p = scale(max_t_year, center = TRUE)) 

temp_scaled %>% 
  ggplot(aes(x = max_t_year, y = scaled_p, colour = site)) + geom_point() +
  theme(legend.position = 'none')

cor.test(temp_scaled$max_t_year, temp_scaled$scaled_p)

set.seed(1)
temp_filtered <- temp_scaled %>% 
  #dplyr::select(-max_t_year) %>% 
  dplyr::rename(t1 = scaled_p) %>% 
  mutate(t2 = t1*2) %>% 
  arrange(t1) %>% 
  ungroup() %>% 
  slice(1, n(), sample(1:n(), 1000)) 



#### all environment ##

library(tidyr)
library(ggplot2)

neonic_long <- environmental_data$neonic_mat %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column("site") %>% 
  pivot_longer(names_to = "year", values_to = "neonic", -site)

pyr_long <- environmental_data$pyr_mat %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column("site") %>% 
  pivot_longer(names_to = "year", values_to = "pyr", -site)

gen_long <- environmental_data$gen_toxic_mat %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column("site") %>% 
  pivot_longer(names_to = "year", values_to = "gen", -site)

tmax_long <- environmental_data$tmax_mat %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column("site") %>% 
  pivot_longer(names_to = "year", values_to = "tmax", -site)

prec_long <- environmental_data$prec_mat %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column("site") %>% 
  pivot_longer(names_to = "year", values_to = "prec", -site)

drought_long <- environmental_data$drought_mat %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column("site") %>% 
  pivot_longer(names_to = "year", values_to = "drought", -site)

ag_long <- environmental_data$ag_mat %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column("site") %>% 
  pivot_longer(names_to = "year", values_to = "ag", -site)

floral_long <- environmental_data$floral_mat %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column("site") %>% 
  pivot_longer(names_to = "year", values_to = "floral", -site)

nest_long <- environmental_data$nesting_mat %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column("site") %>% 
  pivot_longer(names_to = "year", values_to = "nest", -site) 


all_long <- neonic_long %>% 
  left_join(pyr_long) %>% 
  left_join(gen_long) %>% 
  left_join(tmax_long) %>% 
  left_join(prec_long) %>% 
  left_join(drought_long) %>% 
  left_join(ag_long) %>% 
  left_join(floral_long) %>% 
  #left_join(nest_long) %>% 
  select(-site, -year)



cor(all_long)


neonic_long %>% 
  left_join(pyr_long) %>% 
  left_join(gen_long) %>% 
  ggplot(aes(x= gen, y = pyr, colour = year)) + geom_point() +
  facet_wrap(~year)