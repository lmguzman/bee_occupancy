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

get.y.val.main.all <- function(sims.mat, ev){
  
  if(ev == "temp"){
    t1 <- seq(from=min(my.data[[1]]$tmax),  
              to=  max(my.data[[1]]$tmax),
              length.out=1000)
    p <- mean(my.data[[1]]$prec)
    nc <- mean(my.data[[1]]$pesticide1)
    ag <- mean(my.data[[1]]$agriculture)
    
    one_sp_ev_occ <- rbindlist(lapply(1:length(t1), function(x) get.y.val.main(sims.mat, tt1 = t1[x], 
                                                                               pp = p, nn = nc, ag = ag)))
    
    
  }else if(ev == 'prec'){
    
    t1 <- mean(my.data[[1]]$tmax)
    p <- seq(from=min(my.data[[1]]$prec),  
             to=  max(my.data[[1]]$prec),
             length.out=1000)
    nc <- mean(my.data[[1]]$pesticide1)
    ag <- mean(my.data[[1]]$agriculture)
    
    one_sp_ev_occ <- rbindlist(lapply(1:length(p), function(x) get.y.val.main(sims.mat, tt1 = t1, 
                                                                              pp = p[x],  nn = nc, ag = ag)))
    
  }else if(ev == 'neonic'){
    
    t1 <- mean(my.data[[1]]$tmax)
    p <- mean(my.data[[1]]$prec)
    nc <- seq(from=min(my.data[[1]]$pesticide1),  
              to=  max(my.data[[1]]$pesticide1),
              length.out=1000)
    ag <- mean(my.data[[1]]$agriculture)
    
    one_sp_ev_occ <- rbindlist(lapply(1:length(nc), function(x) get.y.val.main(sims.mat, tt1 = t1, 
                                                                               pp = p,  nn = nc[x], ag = ag)))
    
  }else if(ev == 'agriculture'){
    
    t1 <- mean(my.data[[1]]$tmax)
    p <- mean(my.data[[1]]$prec)
    nc <- mean(my.data[[1]]$pesticide1)
    ag <- seq(from=min(my.data[[1]]$agriculture),  
              to=  max(my.data[[1]]$agriculture),
              length.out=1000)
    
    one_sp_ev_occ <- rbindlist(lapply(1:length(ag), function(x) get.y.val.main(sims.mat, tt1 = t1, 
                                                                               pp = p,  nn = nc, ag = ag[x])))
    
  }
  
  return(bind_cols(one_sp_ev_occ, tt1 = t1, pp = p, nn = nc, ag = ag))
}




create_data_for_plots <- function(region){
  
  family <- "ALL"
  year_range <- c(1995, 2015)
  
  # env era
  model <- "ms_env_area_2"
  
  res <- readRDS(paste0("model_outputs/env/res_counties_", paste0(year_range, collapse = "_"), "_",model,"_", family, "_", region, ".rds"))
  
  res.summary <- readRDS(paste0("model_outputs/env/res.summary_counties_", paste0(year_range, collapse = "_"), "_",model,"_", family, "_", region, ".rds"))
  
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
  
  
  env_list <- c('temp', 'prec', 'neonic', 'agriculture')
  

  for(env in env_list){
    print(env)
    
    # main trend occupancy
    
    main_occupancy <- get.y.val.main.all(sims.mat = sims.mat, env)
    
    saveRDS(main_occupancy, paste0('plots/int_data_plots/env/main_',region,"_", env,'_occupancy.rds'))
    
  }
  
}

region_v <- c("Center","SouthEast")

lapply(region_v, create_data_for_plots)


combs_env_rg <- expand.grid(region_v = c("Center","SouthEast"), env_list = c('temp', 'prec', 'neonic', 'agriculture'))

main_region_list <- list()

for(r in 1:nrow(combs_env_rg)){
  
  main_region_list[[r]] <- readRDS(paste0('plots/int_data_plots/env/main_',combs_env_rg$region_v[r],"_", combs_env_rg$env_list[r],'_occupancy.rds')) %>% 
    mutate(region = combs_env_rg$region_v[r], env = combs_env_rg$env_list[r]) %>% 
    as.data.table()
  
}

region_nice_name <- data.frame(region = region_v, region_nice = c("Center", "South East"))

main_trend_prec <- main_region_list %>% 
  rbindlist() %>% 
  left_join(region_nice_name) %>% 
  filter(env == "prec") %>% 
  ggplot() +
  geom_line(aes(x = pp, y = mean)) +
  geom_ribbon(aes(x = pp, ymin = `X2.5.`, ymax = X97.5.), alpha = 0.3) +
  facet_wrap(~region_nice) +
  theme_cowplot() +
  theme(strip.background =element_blank(), 
        axis.text = element_text(size = 15), 
        strip.text = element_text(size = 20), 
        axis.title = element_text(size = 20)) +
  ylab("Occupancy") +
  xlab("Precipitation") 


ggsave(main_trend_prec, filename = "plots/trends/main_env_prec.jpeg")



main_trend_temp <- main_region_list %>% 
  rbindlist() %>% 
  left_join(region_nice_name) %>% 
  filter(env == "temp") %>% 
  ggplot() +
  geom_line(aes(x = tt1, y = mean)) +
  geom_ribbon(aes(x = tt1, ymin = `X2.5.`, ymax = X97.5.), alpha = 0.3) +
  facet_wrap(~region_nice) +
  theme_cowplot() +
  theme(strip.background =element_blank(), 
        axis.text = element_text(size = 15), 
        strip.text = element_text(size = 20), 
        axis.title = element_text(size = 20)) +
  ylab("Occupancy") +
  xlab("Temperature") 


ggsave(main_trend_temp, filename = "plots/trends/main_env_temp.jpeg")



main_trend_agri <- main_region_list %>% 
  rbindlist() %>% 
  left_join(region_nice_name) %>% 
  filter(env == "agriculture") %>% 
  ggplot() +
  geom_line(aes(x = ag, y = mean)) +
  geom_ribbon(aes(x = ag, ymin = `X2.5.`, ymax = X97.5.), alpha = 0.3) +
  facet_wrap(~region_nice) +
  theme_cowplot() +
  theme(strip.background =element_blank(), 
        axis.text = element_text(size = 15), 
        strip.text = element_text(size = 20), 
        axis.title = element_text(size = 20)) +
  ylab("Occupancy") +
  xlab("Percent Agriculture") 


ggsave(main_trend_agri, filename = "plots/trends/main_env_agri.jpeg")




main_trend_neonic <- main_region_list %>% 
  rbindlist() %>% 
  left_join(region_nice_name) %>% 
  filter(env == "neonic") %>% 
  ggplot() +
  geom_line(aes(x = nn, y = mean)) +
  geom_ribbon(aes(x = nn, ymin = `X2.5.`, ymax = X97.5.), alpha = 0.3) +
  facet_wrap(~region_nice) +
  theme_cowplot() +
  theme(strip.background =element_blank(), 
        axis.text = element_text(size = 15), 
        strip.text = element_text(size = 20), 
        axis.title = element_text(size = 20)) +
  ylab("Occupancy") +
  xlab("Neonicotinoid Use") 


ggsave(main_trend_neonic, filename = "plots/trends/main_env_neonic.jpeg")


