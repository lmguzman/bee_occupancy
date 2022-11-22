library(dplyr)
library(data.table)
library(purrr)

## function to prepare occurrence data 

prepare_occurrence <- function(countries, resolution, year_range, family_filter, oc_interval){

  ## load data
  observations_raw <- readRDS(paste0("clean_data/observations/observations_", countries, "_", resolution, ".rds"))
  
  environmental_data <- readRDS(paste0("clean_data/data_prepared/environment_", countries, "_", resolution, "_", paste0(year_range, collapse = "_"),".rds"))
  
  ranges <- readRDS(paste0("clean_data/ranges/ranges_", countries, "_", resolution, ".rds"))
  
  
  ## subset observations to sites in envrivonmental data and year range
  
  observations <- observations_raw[family %in% family_filter & site %in% environmental_data$site_id & year >=  year_range[1] & year <=  year_range[2]]
  
  site_ID <- sort(unique(observations$site))
  
  # clean ranges #
  
  range_site <- lapply(ranges, FUN = function(x) as.character(unlist(x@data)))
  
  sites_by_sp <- lapply(range_site, FUN = function(x) match(intersect(x, site_ID), site_ID))
  
  sites_by_sp <- sites_by_sp[unique(observations$finalName)]
  
  sp_range_df <- data.table(map_df(lapply(sites_by_sp, FUN = function(x) site_ID[x]), ~ data.frame(site = .x), .id = "finalName"))
  
  ## filter observations to only have observations within range 
  
  observations_clean <- inner_join(observations, sp_range_df)
  
  #### get unique data ##
  
  colnames(observations_clean)[7] <- 'month'
  
  year_visit_df <- data.frame(oc_int = paste0("yr",rep(seq(year_range[1], (year_range[2]), oc_interval), each = 2)), 
             year = year_range[1]:year_range[2]) %>%
    left_join(expand.grid(year = year_range[1]:year_range[2], month = 1:12) %>% 
    arrange(year, month) %>%  
    mutate(visit = case_when(month <=6 & year %% 2 != 0 ~ 1,
                             month >6 & year %% 2 != 0 ~ 2,
                             month <=6 & year %% 2 == 0 ~ 3,
                             month >6 & year %% 2 == 0 ~ 4)) %>% 
    mutate(visit = paste0("v", visit))) %>% data.table()
  
  setkeyv(observations_clean, c('year', 'month'))
  setkeyv(year_visit_df, c('year', 'month'))
  
  observations_clean_vis <- year_visit_df[observations_clean]
  
  observations_clean_sp <- distinct(observations_clean_vis[,.(finalName, site, oc_int, genus, visit)])
  
  species_presence <- sort(unique(observations_clean_sp$finalName))
  
  observations_clean_sp$bombus <- ifelse(observations_clean_sp$genus == 'Bombus', "Bombus", "Not Bombus")
  
  site_ID <- sort(unique(observations_clean_sp$site)) 
  yr_ID <- sort(unique(observations_clean_sp$oc_int))
  nsite  <- length(unique(observations_clean_sp$site))
  nyr <- length(unique(observations_clean_sp$oc_int))
  nsp <- length(unique(observations_clean_sp$finalName))
  nvisit <- length(unique(observations_clean_sp$visit))
  
  ## create occupancy array ##
  
  occ.arr <- array(0, dim = c(nsite, nyr, nvisit, nsp), 
                   dimnames = list(site=site_ID,
                                   #year= paste0("yr", seq(1900,2010, 10)),
                                   year= sort(unique(observations_clean_sp$oc_int)),
                                   visit=paste0("v", 1:4),
                                   sp=species_presence))
  
  occ.arr[cbind(match(observations_clean_sp$site, site_ID), match(observations_clean_sp$oc_int, paste0("yr", seq(year_range[1],year_range[2], 2))), match(observations_clean_sp$visit, paste0("v", 1:4)), match(observations_clean_sp$finalName, species_presence))] <- 1 
  
  
  ## subset to species that are present
  sp.keep <- apply(occ.arr, 'sp', sum)>0
  all(sp.keep)
  ## species counts per genus
  
  nsp.arr.gen <- list()
  
  bombus <- c("Bombus", "Not Bombus")
  
  for(b in bombus){
    if(b == "Not Bombus"){
      d <-  c("Not Bombus")
    }else{d <- c("Bombus", "Not Bombus")}
    
    sp_in_genus <- unique(observations_clean_sp[bombus %in% d, finalName])
    sp_n_in_genus <- which(species_presence %in% sp_in_genus)
    nsp.arr <- apply(occ.arr[,,,sp_n_in_genus], c('site','year','visit'), sum)
    vis.arr <- (nsp.arr>0)*1
    nsp.arr.gen[[b]] <- vis.arr
  }
  
  
  
  names(dim(occ.arr)) <- c('nsite', 'nyear', 'nvisit', 'nsp')
  
  sp_gen_directory <- observations_clean_sp[, .(finalName, genus, bombus)] %>% 
    unique() %>% arrange(finalName) %>% mutate(sp_n = 1:n()) 
  
  ## do this by species name instead
  get.indices <- function(sp) {
    vis.arr <- nsp.arr.gen[[sp_gen_directory$bombus[sp]]]
    outside.range <- setdiff(1:dim(occ.arr)['nsite'], sites_by_sp[[sp]])
    vis.arr[outside.range,,] <- 0
    tmp <- which(vis.arr==1, arr.ind=TRUE)
    cbind(sp=rep(sp,nrow(tmp)),tmp)
  }
  
  master.index <-
    do.call(rbind, lapply(1:dim(occ.arr)['nsp'], get.indices))
  
  nrow(master.index)
  
  master.index <- master.index[,c(2,3,4,1)]
  
  ### should be site, yr, sp
  
  X <- occ.arr[master.index]
  
  my.data.era <- list(X=X,
                      yr=master.index[,'year'],
                      site=master.index[,'site'],
                      sp=master.index[,'sp'],
                      nsp=dim(occ.arr)['nsp'],
                      nsite=dim(occ.arr)['nsite'],
                      nyr=dim(occ.arr)['nyear'],
                      nind=nrow(master.index))
  
  my.data.env <- list(X=X,
                  yr=master.index[,'year'],
                  site=master.index[,'site'],
                  sp=master.index[,'sp'],
                  nsp=dim(occ.arr)['nsp'],
                  nsite=dim(occ.arr)['nsite'],
                  nyr=dim(occ.arr)['nyear'],
                  nind=nrow(master.index),
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
                   visit=paste0("v", 1:12),
                   sp=species_presence)
  
  all_data_env <- list(my.data.env, sp_gen_directory, sites_by_sp, 
                       site=site_ID,
                       year= yr_ID,
                       visit=paste0("v", 1:12),
                       sp=species_presence)
  
  saveRDS(all_data_era, paste0("clean_data/data_prepared/my_data_era_",countries, "_", resolution, "_", paste0(year_range, collapse = "_"), "_", family_filter, ".rds" ))

  saveRDS(all_data_env, paste0("clean_data/data_prepared/my_data_env_",countries, "_", resolution, "_", paste0(year_range, collapse = "_"), "_", family_filter,".rds" ))
  
}

## prepare occurrence for US 100

prepare_occurrence("US", 100, c(1997, 2016), "Apidae", 2)


## prepare occurrence for US 50

prepare_occurrence("US", 50, c(1997, 2016),  "Apidae", 2)



