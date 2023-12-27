library(purrr)
library(dplyr)
library(data.table)
library(tidyr)
library(units)

prepare_environmental_data <- function(year_range, length_oc_int){
  
  # prepare temperature 
  
  tmax_mat <- prepare_tmax(year_range, length_oc_int)
  
  # prepare precipitation
  
  prec_mat <- prepare_prec(year_range, length_oc_int)
  
  ## pesticides with area 
  
  both_mat_area <- prepare_pesticide(year_range, "both", area = TRUE, length_oc_int)
  
  ## prepare agriculture
  
  ag_mat <- prepare_agriculture()
  
  ## prepare fraction of animal pollinated in county 
  
  # original non-animal pollinated
  county_fan_mat <- prepare_county_animal(type_filter = "non_animal_pollinated_orig")
  
  #one extreme does not use managed bees
  
  county_fan_mat_mb <- prepare_county_animal(type_filter = "crops_do_not_use_managed_bees")
  
  #other extreme, not attractive to bbees and sol bees
  
  county_fan_mat_abs <- prepare_county_animal(type_filter = "crops_not_attractive_bb_sol")
  
  ## prepare honey bees 
  
  time_col_mat <- prepare_honey_bees_time()
  
  ## get the sites present in all of the matrices
  
  site_id <- sort(Reduce(intersect, list(rownames(tmax_mat), rownames(ag_mat), rownames(time_col_mat), rownames(county_fan_mat))))
  
  nyears <- length(year_range[1]:year_range[2])
  
  final_year <- paste0("yr", year_range[1]:year_range[2])[seq(1,nyears, length_oc_int)]
  
  environment_prepared <- list(tmax_mat = tmax_mat[site_id,final_year],
                               prec_mat = prec_mat[site_id,final_year],
                               ag_mat = ag_mat[site_id,final_year], 
                               both_mat_area = both_mat_area[site_id, final_year],
                               county_fan_mat = county_fan_mat[site_id,],
                               county_fan_mat_mb = county_fan_mat_mb[site_id,],
                               county_fan_mat_abs = county_fan_mat_abs[site_id,],
                               time_col_mat = time_col_mat[site_id, final_year], 
                               site_id = site_id)
  
  saveRDS(environment_prepared, file = paste0("clean_data/data_prepared/environment_counties_", paste0(year_range, collapse = '_'), "_", length_oc_int,".rds"))
  
}


##### temperature #####

prepare_tmax <- function(year_range, length_oc_int){
  
  ## get occupancy and visit periods
  
  nyears <- length(year_range[1]:year_range[2])
  
  n_occ_periods <- round(nyears/length_oc_int)
  
  yr_average <- data.frame(year = year_range[1]:year_range[2], yr_avr = rep(1:n_occ_periods, each = length_oc_int))
  
  sel_years <- yr_average %>% 
    group_by(yr_avr) %>% 
    slice(1) %>% 
    ungroup()
  
  ## load data
  
  climate_raw <- readRDS(paste0("clean_data/climate/climate_counties.rds"))
  
  climate <- climate_raw %>% 
    map_df(~as.data.frame(.x)) %>% 
    filter(year >= year_range[1] & year <= year_range[2]) %>% 
    mutate(year = as.numeric(year))
  
  ## prepare temperature ### 
  
  ## get the right months, summarise, scale and make into a matrix
  
  climate_temp <- climate %>%
    mutate(site = paste0("s_", state_county)) %>% 
    filter(variable == 'tmax') %>% 
    group_by(site, year) %>% 
    summarise(max_t_year = max(values, na.rm = TRUE)/10) %>% 
    left_join(yr_average) %>% 
    group_by(yr_avr, site) %>% 
    summarise(mean_max_t = mean(max_t_year, na.rm = TRUE)) %>% 
    left_join(sel_years) %>% 
    mutate(year = paste0("yr", year)) %>%
    filter(!is.infinite(mean_max_t), !is.na(mean_max_t)) %>% 
    ungroup() %>% 
    mutate(scaled_p = scale(mean_max_t)) %>% 
    dplyr::select(-mean_max_t, -yr_avr) %>% 
    pivot_wider(names_from= 'year', values_from = 'scaled_p') %>% 
    tibble::column_to_rownames("site") %>%
    as.matrix() 
  
  return(climate_temp)
  
}



##### precipitation #####

prepare_prec <- function(year_range, length_oc_int){
  
  ## get occupancy and visit periods
  
  nyears <- length(year_range[1]:year_range[2])
  
  n_occ_periods <- round(nyears/length_oc_int)
  
  yr_average <- data.frame(year = year_range[1]:year_range[2], yr_avr = rep(1:n_occ_periods, each = length_oc_int))
  
  sel_years <- yr_average %>% 
    group_by(yr_avr) %>% 
    slice(1) %>% 
    ungroup()
  
  
  ## load data
  
  climate_raw <- readRDS(paste0("clean_data/climate/climate_counties.rds"))
  
  climate <- climate_raw %>% 
    map_df(~as.data.frame(.x)) %>% 
    filter(year >= year_range[1] & year <= year_range[2]) %>% 
    mutate(year = as.numeric(year))
  
  ## prepare precipitation ### 
  
  ## summarise, scale and make into a matrix
  
  climate_prec <- climate %>% 
    mutate(site = paste0("s_", state_county)) %>% 
    filter(variable == 'prec') %>% 
    group_by(site, year) %>% 
    summarise(mean_prec_year = mean(values, na.rm = TRUE)) %>% 
    left_join(yr_average) %>% 
    group_by(yr_avr, site) %>% 
    summarise(mean_mean_prec = mean(mean_prec_year, na.rm = TRUE)) %>% 
    left_join(sel_years) %>% 
    mutate(year = paste0("yr", year)) %>%
    filter(!is.infinite(mean_mean_prec), !is.na(mean_mean_prec)) %>% 
    ungroup() %>% 
    mutate(scaled_p = scale(mean_mean_prec)) %>% 
    dplyr::select(-mean_mean_prec, -yr_avr) %>% 
    pivot_wider(names_from= 'year', values_from = 'scaled_p') %>% 
    tibble::column_to_rownames("site") %>%
    as.matrix() 
  
  return(climate_prec)
  
}



##### pesticides #####

prepare_pesticide <- function(year_range, pesticide, area, length_oc_int){
  
  ## get occupancy and visit periods
  
  nyears <- length(year_range[1]:year_range[2])
  
  n_occ_periods <- round(nyears/length_oc_int)
  
  yr_average <- data.frame(YEAR = year_range[1]:year_range[2], yr_avr = rep(1:n_occ_periods, each = length_oc_int))
  
  ## load data depending on the pesticide used
  
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
  
  # load sites and area of sites
 
  all_us_sites <- readRDS(paste0("clean_data/sites/sites_counties.rds"))
  
  area_counties <- readRDS("clean_data/sites/area_counties.RDS") %>%
    mutate(site = paste0("s_", state_county)) %>%
    mutate(area_km2 = area_m_2*1e-6)
  
  # filter to the date range
  
  pesticide_raw <- pesticide_raw %>% filter(YEAR >= year_range[1] & YEAR <= year_range[2])
  
  ## fill in gaps for sites where no pesticide use detected 
  
  year_site <- expand.grid(YEAR = year_range[1]:year_range[2], state_county = all_us_sites$state_county,
                           COMPOUND = unique(pesticide_raw$COMPOUND)) %>%  
    data.table()
  
  setkeyv(year_site, c("YEAR", "state_county", "COMPOUND"))
  setkeyv(pesticide_raw,  c("YEAR", "state_county", "COMPOUND"))
  
  ## if pesticide is NA then it gets 0
  
  pesticide_all_sites <- year_site %>% 
    left_join(pesticide_raw) %>% 
    left_join(yr_average) %>% 
    mutate(pest_site_ld50_use = ifelse(is.na(pest_site_ld50), 0, pest_site_ld50)) %>% 
    mutate(site = paste0("s_", state_county)) %>%
    mutate(year = paste0('yr', YEAR)) %>% 
    left_join(area_counties) 
  
  ## divide pesticide by area
  
  if(area == TRUE){
    pesticide_all_sites <- pesticide_all_sites %>% 
      mutate(pest_site_ld50_use = pest_site_ld50_use/drop_units(area_km2))
      
  }
  
  ### average across all years per occupancy periods
  
  averaged_by_year_site_compound <- pesticide_all_sites[,.(av_pesticides = mean(pest_site_ld50_use)), by = .(site, yr_avr, COMPOUND)] %>% 
    left_join(yr_average %>% 
                group_by(yr_avr) %>% 
                slice(1)) %>%
    mutate(year = paste0('yr', YEAR))  
  
  pest_summed <- averaged_by_year_site_compound[,.(summed_pesticides = sum(av_pesticides)), by = .(site, year)]
  
  min_g_0 <- min(pest_summed$summed_pesticides[pest_summed$summed_pesticides>0])
  
  ### log and scale 
  
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

prepare_agriculture <- function(){
  
    ## read in all data
  
    agriculture <- readRDS(paste0("clean_data/agriculture/agriculture_county.rds"))
    
    all_us_sites <- readRDS(paste0("clean_data/sites/sites_counties.rds"))
    
    ## make sure all years and sites have a value
    
    ag_year <- data.frame(new_year = paste0("yr", 1995:2016), year = c(rep(2001, 7), rep(2004, 3), 
                                                                       rep(2006, 2), rep(2008, 2),
                                                                       rep(2011, 3), rep(2013,2),
                                                                       rep(2016, 3)))
    
    year_site <- expand.grid(new_year = paste0('yr', 1995:2016), state_county = all_us_sites$state_county)
    
    
    agriculture_df <- agriculture %>% 
      ### interpolates the same data to every intermediate year based on ag year
      left_join(ag_year) %>% 
      dplyr::select(new_year, state_county, percent_agriculture) %>% 
      full_join(year_site) %>% 
      mutate(site = paste0("s_", state_county)) %>%
      dplyr::select(-state_county)  
    
    ## log and scale
    
    min_ag <- min(agriculture_df$percent_agriculture[agriculture_df$percent_agriculture > 0])
    
    ag_mat <- agriculture_df %>% 
      mutate(ag_log = log(percent_agriculture + (min_ag/2))) %>% 
      dplyr::select(-percent_agriculture) %>% 
      tidyr::pivot_wider(names_from= 'new_year', values_from = 'ag_log') %>% 
      tibble::column_to_rownames("site") %>% 
      as.matrix() 
    
      ag_mat_scaled <- (ag_mat- mean(ag_mat, na.rm = TRUE))/sd(ag_mat, na.rm = TRUE)
    
    
    return(ag_mat_scaled)
  
}



##### Crop animal pollinates #####

prepare_county_animal <- function(type_filter){
  
  ## load data 
  
  crop_county_animal <- readRDS(paste0("clean_data/agriculture/crops_county_animal.rds"))
  
  all_us_sites <- readRDS(paste0("clean_data/sites/sites_counties.rds"))
  
  all_site <- expand.grid(site = paste0("s_", all_us_sites$state_county))
  
  ## total county area
  
  Total_county_area <- crop_county_animal %>% 
    group_by(site) %>% 
    summarise(total_area = sum(Freq))
  
  ## area that is not animal pollinated depending on how it is defined
  
  if(type_filter == "non_animal_pollinated_orig"){
    
    Total_animal_pollinated <- crop_county_animal %>% 
      filter(cover_type == 'Crop') %>% 
      filter(non_animal_pollinated_orig %in% c("FALSE", "HALF")) %>% 
      group_by(site) %>% 
      summarise(total_ani_pollinated = sum(Freq))
    
  }else if(type_filter == "crops_do_not_use_managed_bees"){
    
    Total_animal_pollinated <- crop_county_animal %>% 
      filter(cover_type == 'Crop') %>% 
      filter(crops_do_not_use_managed_bees %in% c("FALSE", "HALF")) %>% 
      group_by(site) %>% 
      summarise(total_ani_pollinated = sum(Freq))
    
  }else if(type_filter == "crops_not_attractive_bb_sol"){
    
    Total_animal_pollinated <- crop_county_animal %>% 
      filter(cover_type == 'Crop') %>% 
      filter(crops_not_attractive_bb_sol %in% c("FALSE", "HALF")) %>% 
      group_by(site) %>% 
      summarise(total_ani_pollinated = sum(Freq))
    
  }
  
  ## calculate the fraction of the county that is "animal pollinated"
  
  frac_animal_pollinated_all <- Total_county_area %>% 
    left_join(Total_animal_pollinated) %>% 
    mutate(county_animal_pol = total_ani_pollinated/total_area)
  
  frac_animal_pol_clean <- frac_animal_pollinated_all %>% 
    dplyr::select(site, county_animal_pol) %>% 
    full_join(all_site) %>% 
    mutate(county_animal_pol = ifelse(is.na(county_animal_pol), 0, county_animal_pol)) 
  
  ## log transform and scale
  
  min_an_pol <- min(frac_animal_pol_clean$county_animal_pol[frac_animal_pol_clean$county_animal_pol > 0])
  
  county_animal_mat <- frac_animal_pol_clean %>% 
    mutate(county_animal_pol = log(county_animal_pol + (min_an_pol/2))) %>% 
    tibble::column_to_rownames("site") %>% 
    as.matrix() 
  
  county_animal_mat_scaled <- (county_animal_mat- mean(county_animal_mat, na.rm = TRUE))/sd(county_animal_mat, na.rm = TRUE)
  
  return(county_animal_mat_scaled)
  
}



prepare_honey_bees_time <- function(){
  
    ## load data 
  
    honey_bees <- readRDS("clean_data/honey_bees/colonies_time.rds")
    
    all_us_sites <- readRDS(paste0("clean_data/sites/sites_counties.rds"))
    
    ## make sure that every site and year have a value
    
    honey_year <- data.frame(new_year = paste0("yr", 1995:2016), Year = c(rep(2002, 8), rep(2007, 5),
                                                                       rep(2012, 5), rep(2017, 4)))
    
    year_site <- expand.grid(new_year = paste0("yr", 1995:2016), state_county = all_us_sites$state_county)
    
    ## load county area
    
    area_counties <- readRDS("clean_data/sites/area_counties.RDS") %>%
      mutate(site = paste0("s_", state_county)) %>%
      mutate(area_km2 = area_m_2*1e-6)
    
    ## make sure every site and year have a value, scale by area of county
    
    honey_bees_all <- honey_bees %>% 
      ### interpolates the same data to every intermediate year based on ag year
      left_join(honey_year) %>% 
      dplyr::select(new_year, state_county, Value) %>% 
      full_join(year_site) %>% 
      mutate(site = paste0("s_", state_county)) %>%
      mutate(Value = ifelse(is.na(Value), 0, Value)) %>% 
      left_join(area_counties) %>% 
      mutate(col_area = Value/drop_units(area_km2)) %>% 
      filter(!is.na(col_area))
    
  ## log transform and scale
    
  min_col <- min(honey_bees_all$col_area[honey_bees_all$col_area > 0])
  
  ## check what is happening here with the honey mat
  honey_mat <- honey_bees_all %>% 
    ### interpolates the same data to every intermediate year based on ag year
    left_join(honey_year) %>% 
    dplyr::select(new_year, state_county, col_area) %>% 
    full_join(year_site) %>% 
    mutate(site = paste0("s_", state_county)) %>%
    mutate(col_area = ifelse(is.na(col_area), 0, col_area)) %>% 
    mutate(col_county = log(col_area + (min_col/2))) %>% 
    dplyr::select(-state_county, -col_area) %>% 
    tidyr::pivot_wider(names_from= 'new_year', values_from = 'col_county') %>% 
    tibble::column_to_rownames("site") %>% 
    as.matrix()
  
  honey_mat_scaled <- (honey_mat- mean(honey_mat, na.rm = TRUE))/sd(honey_mat, na.rm = TRUE)
  
  return(honey_mat_scaled)
  
}
