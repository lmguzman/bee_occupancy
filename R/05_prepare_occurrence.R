## Script to organize the occurrence data and join with envornmental data for model 

library(dplyr)
library(data.table)
library(purrr)
library(stringr)
library(sf)

prepare_occurrence <- function(family_filter, strict_filter, year_range, length_oc_int){
  
  ## load data
  
  observations_raw <- readRDS(paste0("clean_data/observations/observations_counties.rds"))
  
  observations_raw$site <- paste0("s_", observations_raw$state_county)
  
  environmental_data <- readRDS(paste0("clean_data/data_prepared/environment_counties_",paste0(year_range, collapse = '_'), "_", length_oc_int,".rds"))
  
  ranges <- readRDS(paste0("clean_data/ranges/ranges_counties.rds"))
  
  area <- readRDS("clean_data/sites/area_counties.RDS")
  
  environmental_data$site_id2 <- environmental_data$site_id
  
  ## set which counties to use
  
  region_filter <- "ALL"
  
  ## arrange area 
  
  area_df <- area %>% 
    mutate(site = paste0("s_", state_county)) 
  
  area_v <- area_df$area_m_2
  
  names(area_v) <- area_df$site
  
  ## subset observations to sites in environmental data and year range
  
  if(family_filter == 'ALL'){
    observations <- observations_raw[site %in% environmental_data$site_id2 & year >=  year_range[1] & year <=  year_range[2]]
  }else{
    observations <- observations_raw[str_detect(family, family_filter) & site %in% environmental_data$site_id2 & year >=  year_range[1] & year <=  year_range[2]]
  }
  
  site_ID <- sort(unique(observations$site))
  
  # clean ranges #
  
  range_site_clean <- lapply(ranges, FUN = function(x) paste0("s_", x))
  
  sites_by_sp <- range_site_clean[unique(observations$finalName)]
  
  sites_by_sp_nonull <- sites_by_sp[unlist(lapply(sites_by_sp, FUN = function(x) !is.null(x)))]
  
  sp_range_df <- data.table(map_df(sites_by_sp_nonull, ~ data.frame(site = .x), .id = "finalName"))
  
  ## filter observations to only have observations within range 
  
  observations_clean <- inner_join(observations, sp_range_df)
  
  #### add occupancy interval and visit interval  ##

  ## get occupancy and visit periods
  
  year_visit_df <- data.frame(oc_int = paste0("yr",rep(seq(year_range[1], (year_range[2]), length_oc_int), each = length_oc_int)), 
                              year = year_range[1]:year_range[2]) %>%
    mutate(visit = rep(1:length_oc_int, n()/length_oc_int)) %>% 
    mutate(visit = paste0("v", visit)) %>% data.table()
  
  setkey(observations_clean, 'year')
  setkey(year_visit_df, 'year')
  
  observations_clean_vis <- year_visit_df[observations_clean]
  
  ## make sure genus is correct ##
  
  observations_clean_vis$genus <- str_extract(observations_clean_vis$finalName, "[A-Z][a-z]*")
  
  ### remove anything that is the only species in that genus
  
  nsp_genus <- observations_clean_vis %>% 
    dplyr::select(finalName, genus) %>% 
    unique() %>% 
    group_by(genus) %>% 
    summarise(n_sp = n())
  
  genus_moreone <- nsp_genus %>% 
    filter(n_sp > 1)
  
  observations_clean_vis_moreone <- observations_clean_vis %>% 
    filter(genus %in% genus_moreone$genus)
  
  ## get unique observations for each visit interval and species 
  
  observations_clean_sp <- distinct(observations_clean_vis_moreone[,.(finalName, site, oc_int, genus, visit)])
  
  write.csv(observations_clean_sp, paste0("clean_data/observations_used/", family_filter, "_", length_oc_int,".csv"), row.names = FALSE)
  
  
  ## get data to create occupancy array
  
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
  
  ## assign site/visit intervals that were observed as 1s
  
  occ.arr[cbind(match(observations_clean_sp$site, site_ID), match(observations_clean_sp$oc_int, yr_ID), match(observations_clean_sp$visit, visit_ID), match(observations_clean_sp$finalName, species_presence))] <- 1 
  
  ## subset to species that are present
  sp.keep <- apply(occ.arr, 'sp', sum)>0
  all(sp.keep)
  
  ## if needed, use a more strict filtering procedure
  
  if(strict_filter == TRUE){
    
    ## Remove sites that were only sampled in 1 year 
    
    detection_site_year <- apply(occ.arr, c(1,2), sum)
    
    detection_site_year_m1 <- (detection_site_year > 0)*1
    
    sites_with_only_1_year <- names(rowSums(detection_site_year_m1)[rowSums(detection_site_year_m1) == 1])
    
    site_ID <- site_ID[!site_ID %in% sites_with_only_1_year]
    
    occ.arr <- occ.arr[site_ID,,,]
    
    ## remove species that have less than 3 sites in the region
    
    detection_site_species <- apply(occ.arr, c(1,4), sum)
    
    detection_site_species_m1 <- (detection_site_species > 0)*1
    
    species_with_less3_sites <- names(colSums(detection_site_species_m1)[colSums(detection_site_species_m1) <=3 ])
    
    species_presence <- species_presence[!species_presence %in% species_with_less3_sites]
    
    occ.arr <- occ.arr[,,,species_presence]
    
  }
  
  ## get visit array counts per genus
  # this is where we set site/time interval combinations where a species wtihin the same genus has been observed 
  
  # step 1: generate visit array per genus
  
  # create a directory for species x genus in dataset
  sp_gen_directory <-  observations_clean_sp %>% 
    filter(finalName %in% species_presence) %>% 
    dplyr::select(finalName, genus) %>% unique()
  
  # get the unique genera
  genera <- sort(unique(sp_gen_directory$genus))
  
  nsp.arr.gen <- list()
  
  for(g in genera){
    
    # filter for species in that genera
    sp_in_genus <- unique(sp_gen_directory[genus %in% g, finalName])
    
    # add the number of presences across species 
    nsp.arr <- apply(occ.arr[,,,sp_in_genus], c('site','year','visit'), sum)
    
    # if any site/time combination has 1 or more species then set to have been "visited"
    vis.arr <- (nsp.arr>0)*1
    
    nsp.arr.gen[[g]] <- vis.arr
  }
  
  ## making sure that dim and dimnames are set
  occ_dim_names <- dimnames(occ.arr)
  names(dim(occ.arr)) <- c('nsite', 'nyear', 'nvisit', 'nsp')
  dimnames(occ.arr) <- occ_dim_names
  
  ## get the site, year, visit and species combinations where a species should be modelled 
  
  get.indices <- function(sp) {
    
    # filter for the visit array corresponding for each species based on the genera
    vis.arr <- nsp.arr.gen[[sp_gen_directory[sp_gen_directory$finalName==sp,]$genus]]
    
    # find the sites that are outside of a species range 
    outside.range <- setdiff(dimnames(occ.arr)$site, sites_by_sp[[sp]])
    
    # set the sites outside of the species range as not visited 
    vis.arr[outside.range,,] <- 0
    
    # find the index where that species could have been visited 
    tmp <- which(vis.arr==1, arr.ind=TRUE)
    
    # get species index
    sp_n <- which(dimnames(occ.arr)$sp == sp)
    
    # bind the site/time index with the species index
    cbind(sp=rep(sp_n,nrow(tmp)),tmp)
  }
  
  # apply for all species 
  master.index <-
    do.call(rbind, lapply(species_presence, get.indices))
  
  ## number of site/year/species/visit combinations that will be modeled
  nrow(master.index)
  
  master.index <- master.index[,c(2,3,4,1)]
  rownames(master.index) <- NULL
  
  ### extract the 1s, and 0s to be used

  X <- occ.arr[master.index]
  
  ## organize data for occupancy model 
  
  my.data.env <- list(X=X,
                      yr=master.index[,'year'],
                      site=master.index[,'site'],
                      sp=master.index[,'sp'],
                      nsp=dim(occ.arr)['nsp'],
                      nsite=dim(occ.arr)['nsite'],
                      nyr=dim(occ.arr)['nyear'],
                      nind=nrow(master.index),
                      area=area_v[site_ID],
                      tmax = environmental_data$tmax_mat[site_ID,yr_ID], 
                      prec = environmental_data$prec_mat[site_ID,yr_ID],
                      pesticidearea = environmental_data$both_mat_area[site_ID,yr_ID],
                      agriculture = environmental_data$ag_mat[site_ID,yr_ID], 
                      countanimal = environmental_data$county_fan_mat[site_ID],
                      countanimalmb = environmental_data$county_fan_mat_mb[site_ID],
                      countanimalabs = environmental_data$county_fan_mat_abs[site_ID],
                      honeybeetime = environmental_data$time_col_mat[site_ID, yr_ID])
  
  
  all_data_env <- list(my.data.env, sp_gen_directory, sites_by_sp, 
                       site=site_ID,
                       year= yr_ID,
                       visit=paste0("v", 1:nvisit),
                       sp=species_presence)
  
  
  saveRDS(all_data_env, paste0("clean_data/data_prepared/my_data_env_genus_filtered_trait_agriregion_both_pest_area_county_", paste0(year_range, collapse = '_'), "_", length_oc_int, family_filter,"_ALL", strict_filter,".rds" ))
  
}

## prepare occurrence for each family 

fam <- c("Andrenidae", "Apidae", "Halictidae", 
         "Megachilidae", "Colletidae|Melittidae")

length_oc_int <- 2:5

params <- expand.grid(fam = fam, length_oc_int = length_oc_int) %>% 
  left_join(data.frame(length_oc_int = 2:5, year_range_f = c(2014, 2015, 2014, 2014)))
  
for(x in 1:nrow(params)){
  prepare_occurrence(family_filter = as.character(params$fam[x]), strict_filter = FALSE, 
                     year_range = c(1995, params$year_range_f[x]), length_oc_int = params$length_oc_int[x])
}

