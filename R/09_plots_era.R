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

get.y.val.main <- function(sims.mat, yr) {
  chains <- expit(sims.mat[,'mu.psi.0'] +
                    sims.mat[,'mu.psi.yr']    * yr 
  )
  data.table(data.frame(mean=mean(chains), t(quantile(chains, probs=c(0.025,0.975)))))
}


get.y.val.main.all <- function(sims.mat){
  
    all_yr <- unique(my.data[[1]]$yr)
    
    one_sp_ev_occ <- rbindlist(lapply(all_yr, function(x) get.y.val.main(sims.mat = sims.mat, yr = x)))
    
  
  return(bind_cols(one_sp_ev_occ, yr =  all_yr))
}



create_data_for_plots <- function(region){
  
  family <- "ALL"
  year_range <- c(1995, 2015)
  
  # env era
  model <- "ms_era_1_area"
  
  res <- readRDS(paste0("model_outputs/era/res_counties_", paste0(year_range, collapse = "_"), "_",model,"_", family, "_", region, ".rds"))
  
  res.summary <- readRDS(paste0("model_outputs/era/res.summary_counties_", paste0(year_range, collapse = "_"), "_",model,"_", family, "_", region, ".rds"))
  
  my.data <- readRDS(paste0("clean_data/data_prepared/my_data_era_genus_counties_", paste0(year_range, collapse = "_"),  "_", family, "_", region, ".rds"))
  
  
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
  
  
  main_occupancy <- get.y.val.main.all(sims.mat = sims.mat)
  
    saveRDS(main_occupancy, paste0('plots/int_data_plots/era/main_',region,'_occupancy.rds'))
  
}

region_v <- c("West","Center","NorthEast", "SouthEast")

lapply(region_v, create_data_for_plots)


main_region_list <- list()

for(reg in region_v){
  
  main_region_list[[reg]] <- readRDS(paste0('plots/int_data_plots/era/main_',reg,'_occupancy.rds')) %>% 
    mutate(region = reg) %>% 
    as.data.table()
  
}

region_nice_name <- data.frame(region = region_v, region_nice = c("West","Center","North East", "South East"))

main_trend_all <- main_region_list %>% 
  rbindlist() %>% 
  left_join(region_nice_name) %>% 
  ggplot() +
  geom_line(aes(x = yr, y = mean)) +
  geom_ribbon(aes(x = yr, ymin = `X2.5.`, ymax = X97.5.), alpha = 0.3) +
  facet_wrap(~region_nice) +
  theme_cowplot() +
  theme(strip.background =element_blank()) +
  ylab("Occupancy") +
  xlab("") +
  scale_x_continuous(breaks = c(1:7), labels = seq(1995, 2015, 3))
  

ggsave(main_trend_all, filename = "plots/trends/main_era.jpeg")
