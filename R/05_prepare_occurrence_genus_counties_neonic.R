library(dplyr)
library(data.table)
library(purrr)
library(stringr)
library(sf)

## function to prepare occurrence data 

prepare_occurrence <- function( year_range, family_filter, oc_interval, region_filter){
  
  ## load data
  observations_raw <- readRDS(paste0("clean_data/observations/observations_counties.rds"))
  
  observations_raw$site <- paste0("s_", observations_raw$state_county)
  
  environmental_data <- readRDS(paste0("clean_data/data_prepared/environment_counties_", paste0(year_range, collapse = "_"),".rds"))
  
  ranges <- readRDS(paste0("clean_data/ranges/ranges_counties.rds"))
  
  area <- readRDS("clean_data/sites/area_counties.RDS")
  
  sites_with_neonics <-readRDS("clean_data/pesticide/sites_with_neonics.rds")
  
  ## load regions and sites
  
  region_df <- readRDS("clean_data/sites/site_counties_region.rds")
  
  if(region_filter == "ALL"){
    chosen_state_county <- sites_with_neonics
  }else{
    chosen_state_county <- paste0("s_", filter(region_df, region == region_filter)$state_county)
    
  }
  
  environmental_data$site_id2 <- environmental_data$site_id[environmental_data$site_id %in% chosen_state_county]
  
  ## arrange area 
  
  area_df <- area %>% 
    mutate(site = paste0("s_", state_county)) 
  
  area_v <- area_df$area_m_2
  
  names(area_v) <- area_df$site
  
  ## subset observations to sites in envrivonmental data and year range
  
  if(family_filter == 'ALL'){
    observations <- observations_raw[site %in% environmental_data$site_id2 & year >=  year_range[1] & year <=  year_range[2]]
  }else{
    observations <- observations_raw[family %in% family_filter & site %in% environmental_data$site_id2 & year >=  year_range[1] & year <=  year_range[2]]
  }
  
  site_ID <- sort(unique(observations$site))
  
  # clean ranges #
  
  range_site_clean <- lapply(ranges, FUN = function(x) paste0("s_", x))
  
  sites_by_sp <- range_site_clean[unique(observations$finalName)]
  
  sp_range_df <- data.table(map_df(sites_by_sp, ~ data.frame(site = .x), .id = "finalName"))
  
  ## filter observations to only have observations within range 
  
  observations_clean <- inner_join(observations, sp_range_df)
  
  #### get unique data ##
  
  colnames(observations_clean)[7] <- 'month'
  
  year_visit_df <- data.frame(oc_int = paste0("yr",rep(seq(year_range[1], (year_range[2]), oc_interval), each = oc_interval)), 
                              year = year_range[1]:year_range[2]) %>%
    mutate(visit = rep(1:3, n()/3)) %>% 
    # left_join(expand.grid(year = year_range[1]:year_range[2], month = 1:12) %>% 
    #             arrange(year, month) %>%  
    #             mutate(visit = case_when(month <=6 & year %% 2 != 0 ~ 1,
    #                                      month >6 & year %% 2 != 0 ~ 2,
    #                                      month <=6 & year %% 2 == 0 ~ 3,
    #                                      month >6 & year %% 2 == 0 ~ 4)) %>% 
    mutate(visit = paste0("v", visit)) %>% data.table()
  
  # setkeyv(observations_clean, c('year', 'month'))
  # setkeyv(year_visit_df, c('year', 'month'))
  
  setkey(observations_clean, 'year')
  setkey(year_visit_df, 'year')
  
  observations_clean_vis <- year_visit_df[observations_clean]
  
  observations_clean_vis$genus <- str_extract(observations_clean_vis$finalName, "[A-Z][a-z]*")
  
  observations_clean_sp <- distinct(observations_clean_vis[,.(finalName, site, oc_int, genus, visit)])
  
  species_presence <- sort(unique(observations_clean_sp$finalName))
  site_ID <- sort(unique(observations_clean_sp$site)) 
  yr_ID <- sort(unique(observations_clean_sp$oc_int))
  visit_ID <- sort(unique(observations_clean_sp$visit))
  
  nsite  <- length(unique(observations_clean_sp$site))
  nyr <- length(unique(observations_clean_sp$oc_int))
  nsp <- length(unique(observations_clean_sp$finalName))
  nvisit <- length(unique(observations_clean_sp$visit))
  
  ## create occupancy array ##
  
  occ.arr <- array(0, dim = c(nsite, nyr, nvisit, nsp), 
                   dimnames = list(site=site_ID,
                                   year= yr_ID,
                                   visit=visit_ID,
                                   sp=species_presence))
  
  occ.arr[cbind(match(observations_clean_sp$site, site_ID), match(observations_clean_sp$oc_int, yr_ID), match(observations_clean_sp$visit, visit_ID), match(observations_clean_sp$finalName, species_presence))] <- 1 
  
  ## subset to species that are present
  sp.keep <- apply(occ.arr, 'sp', sum)>0
  all(sp.keep)
  ## species counts per genus
  
  nsp.arr.gen <- list()
  
  genera <- sort(unique(observations_clean_sp$genus))
  
  for(g in genera){
    sp_in_genus <- unique(observations_clean_sp[genus %in% g, finalName])
    nsp.arr <- apply(occ.arr[,,,sp_in_genus], c('site','year','visit'), sum)
    vis.arr <- (nsp.arr>0)*1
    nsp.arr.gen[[g]] <- vis.arr
  }
  
  occ_dim_names <- dimnames(occ.arr)
  names(dim(occ.arr)) <- c('nsite', 'nyear', 'nvisit', 'nsp')
  dimnames(occ.arr) <- occ_dim_names
  
  sp_gen_directory <- unique(observations_clean_sp[, .(finalName, genus)]) 
  
  get.indices <- function(sp) {
    vis.arr <- nsp.arr.gen[[sp_gen_directory[sp_gen_directory$finalName==sp,]$genus]]
    outside.range <- setdiff(dimnames(occ.arr)$site, sites_by_sp[[sp]])
    vis.arr[outside.range,,] <- 0
    tmp <- which(vis.arr==1, arr.ind=TRUE)
    sp_n <- which(dimnames(occ.arr)$sp == sp)
    cbind(sp=rep(sp_n,nrow(tmp)),tmp)
  }
  
  master.index <-
    do.call(rbind, lapply(species_presence, get.indices))
  
  nrow(master.index)
  
  master.index <- master.index[,c(2,3,4,1)]
  rownames(master.index) <- NULL
  
  ### should be site, yr, sp
  
  X <- occ.arr[master.index]
  
  my.data.era <- list(X=X,
                      yr=master.index[,'year'],
                      site=master.index[,'site'],
                      sp=master.index[,'sp'],
                      nsp=dim(occ.arr)['nsp'],
                      nsite=dim(occ.arr)['nsite'],
                      nyr=dim(occ.arr)['nyear'],
                      nind=nrow(master.index), 
                      area=area_v[site_ID])
  
  #tmax_min <- environmental_data$tmax_mat[site_ID,yr_ID] - min(environmental_data$tmax_mat[site_ID,yr_ID])
  
  my.data.env <- list(X=X,
                      yr=master.index[,'year'],
                      site=master.index[,'site'],
                      sp=master.index[,'sp'],
                      nsp=dim(occ.arr)['nsp'],
                      nsite=dim(occ.arr)['nsite'],
                      nyr=dim(occ.arr)['nyear'],
                      nind=nrow(master.index),
                      area=area_v[site_ID],
                      #tmax = tmax_min/max(tmax_min),
                      tmax = environmental_data$tmax_mat[site_ID,yr_ID],
                      prec = environmental_data$prec_mat[site_ID,yr_ID],
                      pesticide1 = environmental_data$neonic_mat[site_ID,yr_ID],
                      pesticide2 = environmental_data$gen_toxic_mat[site_ID,yr_ID],
                      pesticide3 = environmental_data$pyr_mat[site_ID,yr_ID],
                      agriculture = environmental_data$ag_mat[site_ID,yr_ID],
                      drought = environmental_data$drought_mat[site_ID,yr_ID],
                      floral = environmental_data$floral_mat[site_ID,yr_ID],
                      nesting = environmental_data$nesting_mat[site_ID,yr_ID])
  
  
  all_data_era <- list(my.data.era, sp_gen_directory, sites_by_sp, 
                       site=site_ID,
                       year= yr_ID,
                       visit=paste0("v", 1:nvisit),
                       sp=species_presence)
  
  all_data_env <- list(my.data.env, sp_gen_directory, sites_by_sp, 
                       site=site_ID,
                       year= yr_ID,
                       visit=paste0("v", 1:nvisit),
                       sp=species_presence)
  
  saveRDS(all_data_era, paste0("clean_data/data_prepared/my_data_era_genus_counties_neonic_", paste0(year_range, collapse = "_"), "_", family_filter, "_", region_filter, ".rds" ))
  
  saveRDS(all_data_env, paste0("clean_data/data_prepared/my_data_env_genus_counties_neonic_", paste0(year_range, collapse = "_"), "_", family_filter,"_", region_filter, ".rds" ))
  
}

## prepare occurrence for US 100

#prepare_occurrence( c(1995, 2014), "ALL", 2, "West")

prepare_occurrence( c(1995, 2015), "ALL", 3, "ALL")

#### 

### only sites with neonics
### normal scale of temperature (-10,0)
#### all of the predictors on detection
### if nothing works, then manipulate data for neonic signal 



