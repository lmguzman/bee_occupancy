library(ggplot2)
library(cowplot)
library(tidyr)
library(dplyr)
library(RColorBrewer)
library(stringr)
library(data.table)
library(purrr)
source("R/src/initialize.R")

get.summ <- function(pars) {
  summ <- round(cbind(
    res.summary$summary$statistics[pars,'Mean',drop=FALSE],
    res.summary$summary$quantiles[pars,c('2.5%', '97.5%'),drop=FALSE],
    Rhat=res.summary$psrf$psrf[pars,1]
  ), digits=3)
  colnames(summ)[1] <- 'mean'
  summ
}


## functions for calculating occupancy for each species at each year

## species specific function

get.y.val <- function(sims.mat, ss, tt1, pp, nn, ag) {
  
  chains <- expit(sims.mat[,'mu.psi.0'] +
                    sims.mat[,sprintf('psi.sp[%d]', ss)]       +
                    sims.mat[,sprintf('psi.tmax1[%s]', ss)]    * tt1 +
                    sims.mat[,sprintf('psi.tmax2[%s]', ss)]    * tt1^2 +
                    sims.mat[,sprintf('psi.prec[%s]', ss)]    * pp  +
                    sims.mat[,sprintf('psi.pest1[%s]', ss)]     * nn +
                    sims.mat[,sprintf('psi.agric[%s]', ss)]     * ag 
  )
  data.table(data.frame(ss, tt1, pp, nn,  ag,  mean=mean(chains), t(quantile(chains, probs=c(0.025,0.975)))))
}

## mean effect function

get.y.val.main <- function(sims.mat, tt1, pp,  nn, ag) {
  chains <- expit(sims.mat[,'mu.psi.0'] +
                    sims.mat[,'mu.psi.tmax1']    * tt1 +
                    sims.mat[,'mu.psi.tmax2']    * tt1^2 +
                    sims.mat[,'mu.psi.prec']   * pp  +
                    sims.mat[,'mu.psi.pest1']    * nn    +
                    sims.mat[,'mu.psi.agric']    * ag    
  )
  data.table(data.frame(mean=mean(chains), t(quantile(chains, probs=c(0.025,0.975)))))
}

## genus specific function

get.y.val.genus <- function(sims.mat, ss, tt1, pp, nn, ag, gg) {
  
  chains <- expit(sims.mat[,'mu.psi.0'] +
                    sims.mat[,sprintf('psi.sp[%d]', ss)]       +
                    sims.mat[,sprintf('psi.tmax1[%s]', ss)]    * tt1 +
                    sims.mat[,sprintf('psi.tmax2[%s]', ss)]    * tt1^2 +
                    sims.mat[,sprintf('psi.prec[%s]', ss)]    * pp  +
                    sims.mat[,sprintf('psi.pest1[%s]', ss)]     * nn +
                    sims.mat[,sprintf('psi.agric[%s]', ss)]     * ag
                  
  )
  
  genus_mean <- rowMeans(chains)
  
  data.table(data.frame(gg, tt1, pp, nn, ag, mean=mean(genus_mean), t(quantile(genus_mean, probs=c(0.025,0.975)))))
}


## calculating occupancy for each species in each decade

get.y.val.all <- function(sims.mat, ev, ss){
  
  if(ev == "temp"){
    
    t1 <- seq(from=min(my.data[[1]]$tmax),  
              to=  max(my.data[[1]]$tmax),
              length.out=1000)
    p <- mean(my.data[[1]]$prec)
    nc <- mean(my.data[[1]]$pesticide1)
    ag <- mean(my.data[[1]]$agriculture)
    
    one_sp_ev_occ <- rbindlist(lapply(1:length(t1), function(x) get.y.val(ss = ss, tt1 = t1[x],
                                                                          pp = p, nn = nc, ag = ag)))
    
    
  }else if(ev == 'prec'){
    
    t1 <- mean(my.data[[1]]$tmax)
    p <- seq(from=min(my.data[[1]]$prec),  
             to=  max(my.data[[1]]$prec),
             length.out=1000)
    nc <- mean(my.data[[1]]$pesticide1)
    ag <- mean(my.data[[1]]$agriculture)
    
    one_sp_ev_occ <- rbindlist(lapply(1:length(p), function(x) get.y.val(ss = ss, tt1 = t1,
                                                                         pp = p[x],  nn = nc, ag = ag)))
    
  }else if(ev == 'neonic'){
    
    t1 <- mean(my.data[[1]]$tmax)
    p <- mean(my.data[[1]]$prec)
    nc <- seq(from=min(my.data[[1]]$pesticide1),  
              to=  max(my.data[[1]]$pesticide1),
              length.out=1000)
    ag <- mean(my.data[[1]]$agriculture)
    
    one_sp_ev_occ <- rbindlist(lapply(1:length(nc), function(x) get.y.val(ss = ss, tt1 = t1, 
                                                                          pp = p, nn = nc[x], ag = ag)))
    
 
  }else if(ev == 'agriculture'){
    
    t1 <- mean(my.data[[1]]$tmax)
    p <- mean(my.data[[1]]$prec)
    nc <- mean(my.data[[1]]$pesticide1)
    ag <- seq(from=min(my.data[[1]]$agriculture),  
              to=  max(my.data[[1]]$agriculture),
              length.out=1000)
    
    one_sp_ev_occ <- rbindlist(lapply(1:length(ag), function(x) get.y.val(ss = ss, tt1 = t1, 
                                                                          pp = p, nn = nc,  ag = ag[x])))
    
  }
  
  return(one_sp_ev_occ)
}



get.y.val.main.all <- function(sims.mat, ev){
  
  if(ev == "temp"){
    t1 <- seq(from=min(my.data[[1]]$tmax),  
              to=  max(my.data[[1]]$tmax),
              length.out=1000)
    p <- mean(my.data[[1]]$prec)
    nc <- mean(my.data[[1]]$pesticide1)
    ag <- mean(my.data[[1]]$agriculture)
    
    one_sp_ev_occ <- rbindlist(lapply(1:length(t1), function(x) get.y.val.main(tt1 = t1[x], 
                                                                               pp = p, nn = nc, ag = ag)))
    
    
  }else if(ev == 'prec'){
    
    t1 <- mean(my.data[[1]]$tmax)
    p <- seq(from=min(my.data[[1]]$prec),  
             to=  max(my.data[[1]]$prec),
             length.out=1000)
    nc <- mean(my.data[[1]]$pesticide1)
    ag <- mean(my.data[[1]]$agriculture)
    
    one_sp_ev_occ <- rbindlist(lapply(1:length(p), function(x) get.y.val.main(tt1 = t1, 
                                                                              pp = p[x],  nn = nc, ag = ag)))
    
  }else if(ev == 'neonic'){
    
    t1 <- mean(my.data[[1]]$tmax)
    p <- mean(my.data[[1]]$prec)
    nc <- seq(from=min(my.data[[1]]$pesticide1),  
              to=  max(my.data[[1]]$pesticide1),
              length.out=1000)
    ag <- mean(my.data[[1]]$agriculture)
    
    one_sp_ev_occ <- rbindlist(lapply(1:length(nc), function(x) get.y.val.main(tt1 = t1, 
                                                                               pp = p,  nn = nc[x], ag = ag)))
    
  }else if(ev == 'agriculture'){
    
    t1 <- mean(my.data[[1]]$tmax)
    p <- mean(my.data[[1]]$prec)
    nc <- mean(my.data[[1]]$pesticide1)
    ag <- seq(from=min(my.data[[1]]$agriculture),  
              to=  max(my.data[[1]]$agriculture),
              length.out=1000)
    
    one_sp_ev_occ <- rbindlist(lapply(1:length(ag), function(x) get.y.val.main(tt1 = t1, 
                                                                               pp = p,  nn = nc, ag = ag[x])))
    
  }
  
  return(bind_cols(one_sp_ev_occ, tt1 = t1, pp = p, nn = nc, ag = ag))
}


get.y.val.all.genus <- function(sims.mat, ev, gg){
  
  ss <- species_directory[genus == gg,]$sp_n
  
  if(ev == "temp"){
    t1 <- seq(from=min(my.data[[1]]$tmax),  
              to=  max(my.data[[1]]$tmax),
              length.out=1000)
    p <- mean(my.data[[1]]$prec)
    nc <- mean(my.data[[1]]$pesticide1)
    ag <- mean(my.data[[1]]$agriculture)
    
    one_sp_ev_occ <- rbindlist(lapply(1:length(t1), function(x) get.y.val.genus(gg = gg, ss = ss, tt1 = t1[x], 
                                                                                pp = p, nn = nc, ag = ag)))
    
    
  }else if(ev == 'prec'){
    
    t1 <- mean(my.data[[1]]$tmax)
    p <- seq(from=min(my.data[[1]]$prec),  
             to=  max(my.data[[1]]$prec),
             length.out=1000)
    nc <- mean(my.data[[1]]$pesticide1)
    ag <- mean(my.data[[1]]$agriculture)
    
    one_sp_ev_occ <- rbindlist(lapply(1:length(p), function(x) get.y.val.genus(gg = gg, ss = ss, tt1 = t1, 
                                                                               pp = p[x],  nn = nc,  ag = ag)))
    
  }else if(ev == 'neonic'){
    
    t1 <- mean(my.data[[1]]$tmax)
    p <- mean(my.data[[1]]$prec)
    nc <- seq(from=min(my.data[[1]]$pesticide1),  
              to=  max(my.data[[1]]$pesticide1),
              length.out=1000)
    ag <- mean(my.data[[1]]$agriculture)
    
    one_sp_ev_occ <- rbindlist(lapply(1:length(nc), function(x) get.y.val.genus(gg = gg, ss = ss, tt1 = t1, 
                                                                                pp = p, nn = nc[x], ag = ag)))
    
  }else if(ev == 'agriculture'){
    
    t1 <- mean(my.data[[1]]$tmax)
    p <- mean(my.data[[1]]$prec)
    nc <- mean(my.data[[1]]$pesticide1)
    ag <- seq(from=min(my.data[[1]]$agriculture),  
              to=  max(my.data[[1]]$agriculture),
              length.out=1000)
    
    one_sp_ev_occ <- rbindlist(lapply(1:length(ag), function(x) get.y.val.genus(gg = gg, ss = ss, tt1 = t1,
                                                                                pp = p, nn = nc,  ag = ag[x])))
    
  }
  
  return(one_sp_ev_occ)
}



### function to run all of the plots

create_data_for_plots <- function(region){
  
  family <- "ALL"
  year_range <- c(1995, 2015)
  
  # env era
  model <- "ms_env_era_area_2"
  
  res <- readRDS(paste0("model_outputs/env_era/res_counties_", paste0(year_range, collapse = "_"), "_",model,"_", family, "_", region, ".rds"))
  
  res <- readRDS(paste0("model_outputs/res_counties_", paste0(year_range, collapse = "_"), "_",model,"_", family, "_", region, ".rds"))
  
  res.summary <- readRDS(paste0("model_outputs/env_era//res.summary_counties_", paste0(year_range, collapse = "_"), "_",model,"_", family, "_", region, ".rds"))
  
  my.data <- readRDS(paste0("clean_data/data_prepared/my_data_env_genus_counties_", paste0(year_range, collapse = "_"),  "_", family, "_", region, ".rds"))
  
  
  vars <- rownames(res.summary$psrf$psrf)
  summ <- get.summ(vars)
  
  summ.paper <- summ[str_detect(rownames(summ), 'mu.psi'),]
  
  
  ####### spaguetti plots per species #######
  
  
  ### assign correct chains ##
  
  sims.mat <- do.call(rbind, res$mcmc)
  sims.arr <-
    aperm(sapply(res$mcmc, I, simplify='array'), c(1,3,2))
  
  nera <- my.data[[1]]$nyr
  nsp <- my.data[[1]]$nsp
  species_directory <- my.data[[2]] %>% 
    mutate(sp_n = 1:n())
    
  #### get species and genus specific effects ###
  
  genus_5_plus <- species_directory %>% 
    group_by(genus) %>% 
    summarise(n_sp = n()) %>% 
    filter(n_sp > 5)
  
  #env_list <- c('temp', 'prec', 'neonic', 'agriculture')
  
  ## temp
  
  env_list <- c('temp')
  
  for(env in env_list){
    print(env)
    
    # species occupancy 
    
    species_occupancy_m18 <- rbindlist(lapply(species_directory[genus %in% genus_5_plus$genus]$sp_n, function(x) get.y.val.all(sims.mat = sims.mat,ev = env, ss = x)))
    setkey(species_occupancy_m18, "ss")
    setkey(species_directory, "sp_n")
    species_occupancy <- species_occupancy_m18[species_directory] %>%
      mutate(region = region)
    saveRDS(species_occupancy, paste0('plots/int_data_plots/species_',region,"_" ,env,'_occupancy.rds'))

    #genus occupancy
    
    genus_occupancy_m18 <- rbindlist(lapply(genus_5_plus$genus, function(x) get.y.val.all.genus(sims.mat = sims.mat, ev = env, gg = x)))
    genus_occupancy <- genus_occupancy_m18 %>%
      mutate(region = region)
    saveRDS(genus_occupancy, paste0('plots/int_data_plots/genus_',region,"_", env,'_occupancy.rds'))

    # main trend occupancy
    
    main_occupancy <- get.y.val.main.all(sims.mat = sims.mat, env)
    
    saveRDS(main_occupancy, paste0('plots/int_data_plots/main_',region,"_", env,'_occupancy.rds'))
    
  }
  
}

### run the prep data for all regions

region_v <- c("Center","NorthEast", "SouthEast")

lapply(region_v, create_data_for_plots)








if(env == 'temp'){
  
  climate <- readRDS("clean_data/climate/climate_counties.rds")
  
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
    mutate(scaled_p = scale(max_t_year, center = TRUE)) %>% 
    filter(site %in% rownames(my.data[[1]]$tmax))
  
  set.seed(1)
  temp_filtered <- temp_scaled %>% 
    #dplyr::select(-max_t_year) %>% 
    dplyr::rename(t1 = scaled_p) %>% 
    mutate(t2 = t1*2) %>% 
    arrange(t1) %>% 
    ungroup() %>% 
    slice(1, n(), sample(1:n(), 1000)) 
}
