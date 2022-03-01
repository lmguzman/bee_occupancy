library(purrr)
library(dplyr)
library(data.table)
library(tidyr)

prepare_environmental_data <- function(countries, resolution, scaling,year_range){
  
  # prepare temperature 
  
  tmax_mat <- prepare_tmax(countries,resolution, scale, year_range, center = TRUE)
  
  # prepare precipitation
  
   prec_mat <- prepare_prec(countries,resolution, scale, year_range, center = TRUE)
  
  ## prepare neonics
   
  neonic_mat <- prepare_pesticide(resolution, year_range, "neonics", 'epest_high')
  
  ## prepare organophosphates
  
  gen_toxic_mat <- prepare_pesticide(resolution, year_range, "gen_toxic", 'epest_high')
  
  ## prepare pyrethroid
  
  pyr_mat <- prepare_pesticide(resolution, year_range, "pyrethroid", 'epest_high')
  
  ## prepare agriculture
  
  ag_mat <- prepare_agriculture(countries, resolution, scaling = scale)
  
  ## prepare drought
  
  drought_mat <- prepare_drought(countries,resolution, year_range)
  
  ## prepare floral
  
  floral_mat <- prepare_floral(countries,resolution, year_range)
  
  ## prepare nesting
  
  nesting_mat <- prepare_nesting(countries,resolution, year_range)
  
  ## get the sites present in all of the matrices
 
  site_id <- sort(Reduce(intersect, list(rownames(nesting_mat),rownames(drought_mat),rownames(tmax_mat), rownames(neonic_mat), rownames(ag_mat))))
  
  environment_prepared <- list(tmax_mat = tmax_mat[site_id,],
                               prec_mat = prec_mat[site_id,],
                               drought_mat = drought_mat[site_id,], 
                               ag_mat = ag_mat[site_id,], 
                               neonic_mat = neonic_mat[site_id,], 
                               gen_toxic_mat = gen_toxic_mat[site_id,], 
                               pyr_mat = pyr_mat[site_id,], 
                               floral_mat = floral_mat[site_id,],
                               nesting_mat = nesting_mat[site_id,], 
                               site_id = site_id)
  
  saveRDS(environment_prepared, file = paste0("clean_data/data_prepared/environment_",countries,"_", resolution, "_", paste0(year_range, collapse = '_'), ".rds"))
  
}



##### temperature #####

prepare_tmax <- function(countries, resolution, scaling, year_range, ...){
  
  climate_raw <- readRDS(paste0("clean_data/climate/climate_",countries,"_", resolution, ".rds"))
  
  climate <- climate_raw %>% 
    map_df(~as.data.frame(.x)) %>% 
    filter(year >= year_range[1] & year <= year_range[2])
  
  ## prepare temperature ### 
  
  climate_temp <- climate %>%
    filter(variable == 'tmax' & month %in% c(7,8)) %>% 
    group_by(site, year) %>% 
    summarise(max_t_year = max(values, na.rm = TRUE)/10) %>% 
    mutate(year = paste0("yr", year)) %>%
    filter(!is.infinite(max_t_year), !is.na(max_t_year)) %>% 
    mutate(scaled_p = scaling(max_t_year, ...)) %>% 
    dplyr::select(-max_t_year) %>% 
    pivot_wider(names_from= 'year', values_from = 'scaled_p') %>% 
    tibble::column_to_rownames("site") %>%
    as.matrix() 
  
  return(climate_temp)
  
}



##### precipitation #####

prepare_prec <- function(countries, resolution, scaling, year_range, ...){
  
  climate_raw <- readRDS(paste0("clean_data/climate/climate_",countries,"_", resolution, ".rds"))
  
  climate <- climate_raw %>% 
    map_df(~as.data.frame(.x)) %>% 
    filter(year >= year_range[1] & year <= year_range[2])
  
  ## prepare temperature ### 
  
  climate_prec <- climate %>% 
    filter(variable == 'prec') %>% 
    group_by(site, year) %>% 
    summarise(mean_prec_year = mean(values, na.rm = TRUE)) %>% 
    mutate(year = paste0("yr", year)) %>%
    filter(!is.infinite(mean_prec_year), !is.na(mean_prec_year)) %>% 
    mutate(scaled_p = scale(mean_prec_year)) %>% 
    dplyr::select(-mean_prec_year) %>% 
    pivot_wider(names_from= 'year', values_from = 'scaled_p') %>% 
    tibble::column_to_rownames("site") %>%
    as.matrix() 
  
  return(climate_prec)
  
}



##### pesticides #####

prepare_pesticide <- function(resolution, year_range, pesticide, epest){
  
  pesticide_raw <- readRDS(paste0("clean_data/pesticide/", pesticide,"_US_", resolution, ".rds"))
  
  if(year_range[1] < min(pesticide_raw$year)){return("year range outside of data bounds")}
  if(year_range[2] > max(pesticide_raw$year)){return("year range outside of data bounds")}
  
  all_us_sites <- readRDS(paste0("clean_data/sites/sites_US_", resolution, ".rds"))
  
  pesticide_raw <- pesticide_raw %>% filter(year >= year_range[1] & year <= year_range[2])
  
  ## fill in gaps for sites where no pesticide use detected 
  
  year_site <- expand.grid(year = year_range[1]:year_range[2], site = all_us_sites$site,
                                  compound = unique(pesticide_raw$compound), epest = epest) %>% 
    data.table()
  
  setkeyv(year_site, c("year", "site", "compound"))
  setkeyv(pesticide_raw,  c("year", "site", "compound"))
  
  pesticide_all_sites <- pesticide_raw[year_site] %>% 
    mutate(epest_high = ifelse(is.na(epest_high), 0, epest_high),
           epest_low = ifelse(is.na(epest_low), 0, epest_low)) 
  
  ### create a matrix 
  
  pesticide_list <- 
    split(pesticide_all_sites, pesticide_all_sites$compound) %>% 
    purrr::map(~mutate(.x, year = paste0("yr", year))) %>% 
    purrr::map(~mutate(.x, logV1 = ifelse(epest == 'epest_high', log(epest_high+1), log(epest_low+1)))) %>%
    #purrr::map(~mutate(.x, scaled_v1 = scaling(logV1))) %>% 
    purrr::map(~dplyr::select(.x, site, year, logV1)) %>%
    purrr::map(~tidyr::pivot_wider(.x, names_from= 'year', values_from = 'logV1')) %>%
    purrr::map(~tibble::column_to_rownames(.x,"site")) %>% 
    purrr::map(~as.matrix(.x)) 
  
  pest_all <- Reduce('+', pesticide_list)
  
  return(pest_all)
}



##### agriculture #####

prepare_agriculture <- function(countries, resolution, scaling, ...){
  
  if(countries == 'US'){
    
    agriculture <- readRDS(paste0("clean_data/agriculture/agriculture_US_", resolution, ".rds"))
    
    all_us_sites <- readRDS(paste0("clean_data/sites/sites_US_", resolution, ".rds"))
    
    ag_year <- data.frame(new_year = paste0("yr", 1995:2016), year = c(rep(2001, 7), rep(2004, 3), 
                                                                       rep(2006, 2), rep(2008, 2),
                                                                       rep(2011, 3), rep(2013,2),
                                                                       rep(2016, 3)))
    
    year_site <- expand.grid(new_year = paste0('yr', 1995:2016), site = all_us_sites$site)
    
    agriculture_mat <- agriculture %>% 
      left_join(ag_year) %>% 
      dplyr::select(new_year, site, scaled_crop_units) %>% 
      full_join(year_site) %>% 
      tidyr::pivot_wider(names_from= 'new_year', values_from = 'scaled_crop_units', values_fill = 0) %>% 
      tibble::column_to_rownames("site") %>% 
      as.matrix()
    
    agriculture_mat[is.na(agriculture_mat)] <- 0
    
    return(agriculture_mat)
    
  }
  
}



##### drought #####

prepare_drought <- function(countries, resolution, year_range, ...){
  
  if(countries == 'US'){
    
    drought_raw <- readRDS(paste0("clean_data/drought/drought_US_", resolution, ".rds"))
    
    drought <- drought_raw %>% 
      filter(year >= year_range[1] & year <= year_range[2])
    
    drought_mat <- drought %>% 
      mutate(year = paste0("yr", year)) %>%
      filter(!is.infinite(mean_drought), !is.na(mean_drought)) %>% 
      dplyr::select(site, year, mean_drought) %>% 
      pivot_wider(names_from= 'year', values_from = 'mean_drought') %>% 
      tibble::column_to_rownames("site") %>%
      as.matrix() 
    
    return(drought_mat)
  }
  
}


##### floral #####

prepare_floral <- function(countries, resolution, scaling, year_range, ...){
  
    floral <- readRDS(paste0("clean_data/land_use/land_use_",countries,"_", resolution, "_floral_all.rds"))

    
    fl_year <- data.frame(new_year = paste0("yr", 1995:2016), year = c(rep(1990, 5), 2000:2016))
    
    floral_mat <- floral %>% 
      left_join(fl_year) %>% 
      filter(!is.na(new_year)) %>% 
      dplyr::select(site, new_year, floral_all) %>% 
      tidyr::pivot_wider(names_from= 'new_year', values_from = 'floral_all') %>% 
      tibble::column_to_rownames("site") %>%
      as.matrix() 
    
    return(floral_mat)
  
}



##### nesting #####

prepare_nesting <- function(countries, resolution, scaling, year_range, ...){
  
  nesting <- readRDS(paste0("clean_data/land_use/land_use_",countries,"_", resolution, "_nesting_all.rds"))

  ns_year <- data.frame(new_year = paste0("yr", 1995:2016), year = c(rep(1990, 5), 2000:2016))
  
  nesting_mat <- nesting %>% 
    left_join(ns_year) %>% 
    filter(!is.na(new_year)) %>% 
    dplyr::select(site, new_year, nesting_all) %>% 
    tidyr::pivot_wider(names_from= 'new_year', values_from = 'nesting_all') %>% 
    tibble::column_to_rownames("site") %>%
    as.matrix() 
  
  return(nesting_mat)
  
}







environment_prep_functions <- function()
  list(climate = prepare_climate, agriculture = prepare_agriculture)

use_environment <- function(variable1){
  unlist(prey_number_stoch()[[variable1]])
}

fun_range <- function(x) {                          
  (x - min(x)) / (max(x) - min(x))
}



identity <- function(x) {                          
  x
}
