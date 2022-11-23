library(purrr)
library(dplyr)
library(data.table)
library(tidyr)

prepare_environmental_data <- function(scaling,year_range){
  
  # prepare temperature 
  
  tmax_mat <- prepare_tmax(scale, year_range, center = TRUE)
  
  # prepare precipitation
  
  prec_mat <- prepare_prec(scale, year_range, center = TRUE)
  
  ## prepare neonics
  
  neonic_mat <- prepare_pesticide(year_range, "neonics")
  
  ## prepare agriculture
  
  ag_mat <- prepare_agriculture(scaling = scale)
  
  ## get the sites present in all of the matrices
  
  site_id <- sort(Reduce(intersect, list(rownames(tmax_mat), rownames(neonic_mat), rownames(ag_mat))))
  
  final_year <- paste0("yr", year_range[1]:year_range[2])
  
  environment_prepared <- list(tmax_mat = tmax_mat[site_id,final_year],
                               prec_mat = prec_mat[site_id,final_year],
                               ag_mat = ag_mat[site_id,final_year], 
                               neonic_mat = neonic_mat[site_id,final_year], 
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

prepare_pesticide <- function(year_range, pesticide){
  
  pesticide_raw <- readRDS(paste0("clean_data/pesticide/", pesticide,"_US_county.rds")) %>% 
    as.data.table()
  
  # if(year_range[1] < min(pesticide_raw$YEAR)){return("year range outside of data bounds")}
  # if(year_range[2] > max(pesticide_raw$YEAR)){return("year range outside of data bounds")}
  # 
  all_us_sites <- readRDS(paste0("clean_data/sites/sites_counties.rds"))
  
  pesticide_raw <- pesticide_raw %>% filter(YEAR >= year_range[1] & YEAR <= year_range[2])
  
  ## fill in gaps for sites where no pesticide use detected 
  
  year_site <- expand.grid(YEAR = year_range[1]:year_range[2], state_county = all_us_sites$state_county,
                           COMPOUND = unique(pesticide_raw$COMPOUND)) %>% 
    data.table()
  
  setkeyv(year_site, c("YEAR", "state_county", "COMPOUND"))
  setkeyv(pesticide_raw,  c("YEAR", "state_county", "COMPOUND"))
  
  pesticide_all_sites <- pesticide_raw[year_site] %>% 
    mutate(pest_site_ld50 = ifelse(is.na(pest_site_ld50), 0, pest_site_ld50)) %>% 
    #mutate(site = paste0("s", str_pad(str_remove(site,"s"), width = 3, pad = "0", side = 'left'))) %>% 
    mutate(site = paste0("s_", state_county)) %>%
    mutate(year = paste0('yr', YEAR))
  
  ### create a matrix 
  
  pest_all <- pesticide_all_sites[,.(summed_pesticides = sum(pest_site_ld50)), by = .(site, year)] %>% 
    mutate(loged_summed_pest = log(summed_pesticides + 1)) %>% 
    dplyr::select(-summed_pesticides) %>% 
    pivot_wider(names_from= 'year', values_from = 'loged_summed_pest') %>% 
    tibble::column_to_rownames("site") %>% 
    as.matrix() 
  
  pest_all_scaled <- (pest_all- mean(pest_all))/sd(pest_all)
  
  # 
  # pesticide_list <- 
  #   split(pesticide_all_sites, pesticide_all_sites$compound) %>% 
  #   purrr::map(~mutate(.x, year = paste0("yr", year))) %>% 
  #   purrr::map(~mutate(.x, logV1 = ifelse(epest == 'epest_high', log(epest_high+1), log(epest_low+1)))) %>%
  #   #purrr::map(~mutate(.x, scaled_v1 = scaling(logV1))) %>% 
  #   purrr::map(~dplyr::select(.x, site, year, logV1)) %>%
  #   purrr::map(~tidyr::pivot_wider(.x, names_from= 'year', values_from = 'logV1')) %>%
  #   purrr::map(~tibble::column_to_rownames(.x,"site")) %>% 
  #   purrr::map(~as.matrix(.x)) 
  
  #pest_all <- Reduce('+', pesticide_list)
  
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



