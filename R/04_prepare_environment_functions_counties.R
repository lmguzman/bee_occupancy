library(purrr)
library(dplyr)
library(data.table)
library(tidyr)
library(units)

prepare_environmental_data <- function(scaling,year_range){
  
  # prepare temperature 
  
  tmax_mat <- prepare_tmax(scale, year_range, center = TRUE)
  
  # prepare precipitation
  
  prec_mat <- prepare_prec(scale, year_range, center = TRUE)
  
  ## prepare pesticides
  
  neonic_mat <- prepare_pesticide(year_range, "neonics", area = FALSE)
  
  pyr_mat <- prepare_pesticide(year_range, "pyr", area = FALSE)
  
  both_mat <- prepare_pesticide(year_range, "both", area = FALSE)
  
  ## pesticides with area 
  
  neonic_mat_area <- prepare_pesticide(year_range, "neonics", area = TRUE)
  
  pyr_mat_area <- prepare_pesticide(year_range, "pyr", area = TRUE)
  
  both_mat_area <- prepare_pesticide(year_range, "both", area = TRUE)
  
  ## prepare agriculture
  
  ag_mat <- prepare_agriculture(scaling = scale)
  
  ## prepare fraction of animal pollinated in ag land
  
  fan_mat <- prepare_fraction_animal(scaling = scale)
  
  ## prepare fraction of animal pollinated in county 
  
  county_fan_mat <- prepare_county_animal(scaling = scale)
  
  ## prepare honey bees 
  
  simple_col_mat <- prepare_honey_bees(scaling = scale, type = "simple")
  
  canag_col_mat <- prepare_honey_bees(scaling = scale, type = "canag")
  
  
  ## get the sites present in all of the matrices
  
  site_id <- sort(Reduce(intersect, list(rownames(tmax_mat), rownames(neonic_mat), rownames(ag_mat), rownames(simple_col_mat), rownames(county_fan_mat))))
  
  final_year <- paste0("yr", year_range[1]:year_range[2])
  
  environment_prepared <- list(tmax_mat = tmax_mat[site_id,final_year],
                               prec_mat = prec_mat[site_id,final_year],
                               ag_mat = ag_mat[site_id,final_year], 
                               neonic_mat = neonic_mat[site_id,final_year], 
                               pyr_mat = pyr_mat[site_id, final_year],
                               both_mat = both_mat[site_id, final_year],
                               neonic_mat_area = neonic_mat_area[site_id, final_year],
                               pyr_mat_area = pyr_mat_area[site_id, final_year],
                               both_mat_area = both_mat_area[site_id, final_year],
                               fan_mat = fan_mat[site_id,],
                               county_fan_mat = county_fan_mat[site_id,],
                               simple_col_mat = simple_col_mat[site_id,],
                               canag_col_mat = canag_col_mat[site_id,],
                               site_id = site_id)
  
  saveRDS(environment_prepared, file = paste0("clean_data/data_prepared/environment_counties_", paste0(year_range, collapse = '_'), ".rds"))
  
}



##### temperature #####

prepare_tmax <- function(scaling, year_range, ...){
  
  climate_raw <- readRDS(paste0("clean_data/climate/climate_counties.rds"))
  
  climate <- climate_raw %>% 
    map_df(~as.data.frame(.x)) %>% 
    filter(year >= year_range[1] & year <= year_range[2])
  
  ## prepare temperature ### 
  
  climate_temp <- climate %>%
    #mutate(site = paste0("s", str_pad(str_remove(site,"s"), width = 3, pad = "0", side = 'left'))) %>%
    mutate(site = paste0("s_", state_county)) %>% 
    filter(variable == 'tmax' & month %in% c(7,8)) %>% 
    group_by(site, year) %>% 
    summarise(max_t_year = max(values, na.rm = TRUE)/10) %>% 
    mutate(year = paste0("yr", year)) %>%
    filter(!is.infinite(max_t_year), !is.na(max_t_year)) %>% 
    ungroup() %>% 
    mutate(scaled_p = scaling(max_t_year)) %>% 
    dplyr::select(-max_t_year) %>% 
    pivot_wider(names_from= 'year', values_from = 'scaled_p') %>% 
    tibble::column_to_rownames("site") %>%
    as.matrix() 
  
  return(climate_temp)
  
}



##### precipitation #####

prepare_prec <- function(scaling, year_range, ...){
  
  climate_raw <- readRDS(paste0("clean_data/climate/climate_counties.rds"))
  
  climate <- climate_raw %>% 
    map_df(~as.data.frame(.x)) %>% 
    filter(year >= year_range[1] & year <= year_range[2])
  
  ## prepare temperature ### 
  
  climate_prec <- climate %>% 
    #mutate(site = paste0("s", str_pad(str_remove(site,"s"), width = 3, pad = "0", side = 'left'))) %>% 
    mutate(site = paste0("s_", state_county)) %>% 
    filter(variable == 'prec') %>% 
    group_by(site, year) %>% 
    summarise(mean_prec_year = mean(values, na.rm = TRUE)) %>% 
    mutate(year = paste0("yr", year)) %>%
    filter(!is.infinite(mean_prec_year), !is.na(mean_prec_year)) %>% 
    ungroup() %>% 
    mutate(scaled_p = scaling(mean_prec_year)) %>% 
    dplyr::select(-mean_prec_year) %>% 
    pivot_wider(names_from= 'year', values_from = 'scaled_p') %>% 
    tibble::column_to_rownames("site") %>%
    as.matrix() 
  
  return(climate_prec)
  
}



##### pesticides #####

prepare_pesticide <- function(year_range, pesticide, area){
  
  if(pesticide == 'both'){
    
    neonic_raw <- readRDS("clean_data/pesticide/neonics_US_county.rds") %>% 
      as.data.table()
    
    pyr_raw <- readRDS("clean_data/pesticide/pyr_US_county.rds")%>% 
      as.data.table()
    
    pesticide_raw <- rbind(neonic_raw, pyr_raw)
    
  }else{
    pesticide_raw <- readRDS(paste0("clean_data/pesticide/", pesticide,"_US_county.rds")) %>% 
      as.data.table()
  }
 
  all_us_sites <- readRDS(paste0("clean_data/sites/sites_counties.rds"))
  
  area_counties <- readRDS("clean_data/sites/area_counties.RDS") %>%
    mutate(site = paste0("s_", state_county)) %>%
    mutate(area_km2 = area_m_2*1e-6)
  
  # crop_area_counties <- readRDS("clean_data/agriculture/crop_cover_county.rds") %>% 
  #   mutate(area_m2 = freq_all*900) %>% 
  #   mutate(area_km2 = area_m2*1e-6)
  
  pesticide_raw <- pesticide_raw %>% filter(YEAR >= year_range[1] & YEAR <= year_range[2])
  
  ## fill in gaps for sites where no pesticide use detected 
  
  year_site <- expand.grid(YEAR = year_range[1]:year_range[2], state_county = all_us_sites$state_county,
                           COMPOUND = unique(pesticide_raw$COMPOUND)) %>% 
    data.table()
  
  setkeyv(year_site, c("YEAR", "state_county", "COMPOUND"))
  setkeyv(pesticide_raw,  c("YEAR", "state_county", "COMPOUND"))
  
  pesticide_all_sites <- year_site %>% 
    left_join(pesticide_raw) %>% 
    mutate(pest_site_ld50_use = ifelse(is.na(pest_site_ld50), 0, pest_site_ld50)) %>% 
    #mutate(site = paste0("s", str_pad(str_remove(site,"s"), width = 3, pad = "0", side = 'left'))) %>% 
    mutate(site = paste0("s_", state_county)) %>%
    mutate(year = paste0('yr', YEAR)) %>% 
    left_join(area_counties) 
  
  if(area == TRUE){
    pesticide_all_sites <- pesticide_all_sites %>% 
      mutate(pest_site_ld50_use = pest_site_ld50_use/drop_units(area_km2))
      
  }
  
  ### create a matrix 
  
  pest_summed <- pesticide_all_sites[,.(summed_pesticides = sum(pest_site_ld50_use)), by = .(site, year)]
  
  min_g_0 <- min(pest_summed$summed_pesticides[pest_summed$summed_pesticides>0])
  
  pest_all <-  pest_summed %>% 
    mutate(loged_summed_pest = log(summed_pesticides + (min_g_0/2))) %>% 
    dplyr::select(-summed_pesticides) %>% 
    pivot_wider(names_from= 'year', values_from = 'loged_summed_pest') %>% 
    tibble::column_to_rownames("site") %>% 
    as.matrix() 
  
  pest_all_scaled <- (pest_all- mean(pest_all, na.rm = TRUE))/sd(pest_all, na.rm = TRUE)
  
  return(pest_all_scaled)
}



##### agriculture #####

prepare_agriculture <- function(scaling, ...){
  
    agriculture <- readRDS(paste0("clean_data/agriculture/agriculture_county.rds"))
    
    all_us_sites <- readRDS(paste0("clean_data/sites/sites_counties.rds"))
    
    ag_year <- data.frame(new_year = paste0("yr", 1995:2016), year = c(rep(2001, 7), rep(2004, 3), 
                                                                       rep(2006, 2), rep(2008, 2),
                                                                       rep(2011, 3), rep(2013,2),
                                                                       rep(2016, 3)))
    
    year_site <- expand.grid(new_year = paste0('yr', 1995:2016), state_county = all_us_sites$state_county)
    
  
    agriculture_mat <- agriculture %>% 
      ### interpolates the same data to every intermediate year based on ag year
      left_join(ag_year) %>% 
      dplyr::select(new_year, state_county, percent_agriculture) %>% 
      full_join(year_site) %>% 
      mutate(site = paste0("s_", state_county)) %>%
      dplyr::select(-state_county) %>% 
      tidyr::pivot_wider(names_from= 'new_year', values_from = 'percent_agriculture') %>% 
      tibble::column_to_rownames("site") %>% 
      as.matrix()
    
    return(agriculture_mat)
  
}



##### Fraction animal pollinates #####

prepare_fraction_animal <- function(scaling, ...){
  
  crop_county_animal <- readRDS(paste0("clean_data/agriculture/crops_county_animal.rds"))
  
  all_us_sites <- readRDS(paste0("clean_data/sites/sites_counties.rds"))
  
  all_site <- expand.grid(site = paste0("s_", all_us_sites$state_county))
  
  
  Total_agricultural_area <- crop_county_animal %>% 
    filter(cover_type == 'Crop') %>% 
    group_by(site) %>% 
    summarise(total_ag_area = sum(Freq))
  
  Total_animal_pollinated <- crop_county_animal %>% 
    filter(cover_type == 'Crop') %>% 
    filter(non_animal_pollinated %in% c("FALSE", "HALF")) %>% 
    group_by(site) %>% 
    summarise(total_ani_pollinated = sum(Freq))
  
  frac_animal_pollinated_all <- Total_agricultural_area %>% 
    left_join(Total_animal_pollinated) %>% 
    mutate(frac_animal_pol = total_ani_pollinated/total_ag_area)
  
  frac_animal_mat <- frac_animal_pollinated_all %>% 
    dplyr::select(site, frac_animal_pol) %>% 
    full_join(all_site) %>%
    mutate(frac_animal_pol = ifelse(is.na(frac_animal_pol), 0, frac_animal_pol)) %>% 
    tibble::column_to_rownames("site") %>% 
    as.matrix()
  
  return(frac_animal_mat)
  
}



##### Crop animal pollinates #####

prepare_county_animal <- function(scaling, ...){
  
  crop_county_animal <- readRDS(paste0("clean_data/agriculture/crops_county_animal.rds"))
  
  all_us_sites <- readRDS(paste0("clean_data/sites/sites_counties.rds"))
  
  all_site <- expand.grid(site = paste0("s_", all_us_sites$state_county))
  
  
  Total_county_area <- crop_county_animal %>% 
    group_by(site) %>% 
    summarise(total_area = sum(Freq))
  
  Total_animal_pollinated <- crop_county_animal %>% 
    filter(cover_type == 'Crop') %>% 
    filter(non_animal_pollinated %in% c("FALSE", "HALF")) %>% 
    group_by(site) %>% 
    summarise(total_ani_pollinated = sum(Freq))
  
  frac_animal_pollinated_all <- Total_county_area %>% 
    left_join(Total_animal_pollinated) %>% 
    mutate(county_animal_pol = total_ani_pollinated/total_area)
  
  frac_animal_pol_clean <- frac_animal_pollinated_all %>% 
    dplyr::select(site, county_animal_pol) %>% 
    full_join(all_site) %>% 
    mutate(county_animal_pol = ifelse(is.na(county_animal_pol), 0, county_animal_pol)) 
  
  min_an_pol <- min(frac_animal_pol_clean$county_animal_pol[frac_animal_pol_clean$county_animal_pol > 0])
  
  county_animal_mat <- frac_animal_pol_clean %>% 
    mutate(county_animal_pol = log(county_animal_pol + (min_an_pol/2))) %>% 
    tibble::column_to_rownames("site") %>% 
    as.matrix() 
  
  county_animal_mat_scaled <- (county_animal_mat- mean(county_animal_mat, na.rm = TRUE))/sd(county_animal_mat, na.rm = TRUE)
  
  return(county_animal_mat_scaled)
  
}



##### honey bees #####

prepare_honey_bees <- function(scaling, type){
  
  if(type == 'simple'){
    honey_bees <- readRDS("clean_data/honey_bees/colonies_county_simple.rds")
  }else if(type == 'canag'){
    honey_bees <- readRDS("clean_data/honey_bees/colonies_county_canag.rds")
    
  }
  
  honey_bees$col_county
  
  min_col <- min(honey_bees$col_county[honey_bees$col_county > 0])
  
  county_colony_mat <- honey_bees %>% 
    mutate(col_county = log(col_county + (min_col/2))) %>% 
    tibble::column_to_rownames("site") %>% 
    as.matrix() 
  
  county_colony_mat_scaled <- (county_colony_mat- mean(county_colony_mat, na.rm = TRUE))/sd(county_colony_mat, na.rm = TRUE)
  
  return(county_colony_mat_scaled)
  
}
