
get.summ <- function(pars, res.summary) {
  summ <- round(cbind(
    res.summary$summary$statistics[pars,'Mean',drop=FALSE],
    res.summary$summary$quantiles[pars,c('2.5%', '97.5%'),drop=FALSE],
    Rhat=res.summary$psrf$psrf[pars,1]
  ), digits=3)
  colnames(summ)[1] <- 'mean'
  summ
}

### species specific function 

get.y.val <- function(sims.mat, ss, tt1, pp, nn, ag, yr, region) {
  
  chains <- expit(sims.mat[,'mu.psi.0'] +
                    sims.mat[,sprintf('psi.sp[%d]', ss)]       +
                    sims.mat[,sprintf('psi.tmax1[%s]', ss)]    * tt1 +
                    sims.mat[,sprintf('psi.tmax2[%s]', ss)]    * tt1^2 +
                    sims.mat[,sprintf('psi.prec[%s]', ss)]    * pp  +
                    sims.mat[,sprintf('psi.pest1[%s]', ss)]     * nn +
                    sims.mat[,sprintf('psi.agric[%s]', ss)]     * ag 
  )

  if(!is.na(region)){
    saveRDS(chains, paste0("plots/int_data_plots/env/chains/", ss, "_", yr, "_", region, ".rds"))
  }
  data.table(data.frame(ss, tt1, pp, nn,  ag, yr, mean=mean(chains), t(quantile(chains, probs=c(0.025,0.975)))))
}


get.y.val.ns <- function(sims.mat, ss, tt1, pp, nn, ag, yr, region) {
  
  chains <- expit(sims.mat[,'mu.psi.0'] +
                    sims.mat[,sprintf('psi.sp[%d]', ss)]       +
                    sims.mat[,sprintf('psi.tmax1[%s]', ss)]    * tt1 +
                    sims.mat[,sprintf('psi.tmax2[%s]', ss)]    * tt1^2 +
                    sims.mat[,sprintf('psi.prec[%s]', ss)]    * pp  +
                    sims.mat[,sprintf('psi.pest1[%s]', ss)]     * nn +
                    sims.mat[,sprintf('psi.agric[%s]', ss)]     * ag 
  )
  
  data.table(data.frame(ss, tt1, pp, nn,  ag, yr, chains))
}



## calculating occupancy for all species for a certain environment ###

get.y.val.all <- function(sims.mat, ss, my.data, yr, species_directory, region = region){
    

    sites_sp <- my.data[[3]][[filter(species_directory, sp_n == ss)$finalName]]
  
    sites_in_region <- rownames(my.data[[1]]$tmax)
    
    species_sites_in_region <- sites_sp[sites_sp %in% sites_in_region]
  
    t1 <- mean(my.data[[1]]$tmax[species_sites_in_region, yr])
    p <- mean(my.data[[1]]$prec[species_sites_in_region, yr])
    nc <- mean(my.data[[1]]$pesticide1[species_sites_in_region, yr])
    ag <- mean(my.data[[1]]$agriculture[species_sites_in_region, yr])
    
    one_sp_ev_occ <- get.y.val.ns(sims.mat, ss = ss, tt1 = t1, pp = p, nn = nc,  ag = ag, yr = yr, region = region)
    

  
  return(one_sp_ev_occ)
}





get.y.val.all.neonic <- function(sims.mat, ss, my.data, yr, species_directory, region = region){
  
  ## Varying only neonics
  
  sites_sp <- my.data[[3]][[filter(species_directory, sp_n == ss)$finalName]]
  
  sites_in_region <- rownames(my.data[[1]]$tmax)
  
  species_sites_in_region <- sites_sp[sites_sp %in% sites_in_region]
  
  t1 <- mean(my.data[[1]]$tmax[species_sites_in_region, yr])
  p <- mean(my.data[[1]]$prec[species_sites_in_region, yr])
  nc <- min(my.data[[1]]$pesticide1)
  ag <- mean(my.data[[1]]$agriculture[species_sites_in_region, yr])
  
  one_sp_ev_occ <- get.y.val.ns(sims.mat, ss = ss, tt1 = t1, pp = p, nn = nc,  ag = ag, yr = yr, region = region)
  
  
  
  return(one_sp_ev_occ)
}



## instead of selecting all of the sites in the range, select the values in 
## of the most sprayed sites
## select the most sprayed sites
## for each species we selected the most impacted sites
## sites in a range that had the biggest change
## vs the sites where the neonics changed the least contrast with the sites that changed the most
# top 25 % vs bottom 25 %

get.y.val.all.top.bottom <- function(sims.mat, ss, my.data, yr, species_directory, region = region){
  
  
  sites_sp <- my.data[[3]][[filter(species_directory, sp_n == ss)$finalName]]
  
  sites_in_region <- rownames(my.data[[1]]$tmax)
  
  species_sites_in_region <- sites_sp[sites_sp %in% sites_in_region]
  
  neonic_values_range <- sort(my.data[[1]]$pesticide1[species_sites_in_region, 7] - my.data[[1]]$pesticide1[species_sites_in_region, 1])  
  
  n_sites <- round(length(neonic_values_range)/4)
  
  sites_bottom_25 <- names(neonic_values_range[1:n_sites])
  
  sites_top_25 <- names(neonic_values_range[length(neonic_values_range):(length(neonic_values_range)-n_sites)])
  
  t1 <- mean(my.data[[1]]$tmax[sites_bottom_25, yr])
  p <- mean(my.data[[1]]$prec[sites_bottom_25, yr])
  nc <- mean(my.data[[1]]$pesticide1[sites_bottom_25, yr])
  ag <- mean(my.data[[1]]$agriculture[sites_bottom_25, yr])
  
  one_sp_ev_occ_bottom <- get.y.val.ns(sims.mat, ss = ss, tt1 = t1, pp = p, nn = nc,  ag = ag, yr = yr, region = region)
  
  t1 <- mean(my.data[[1]]$tmax[sites_top_25, yr])
  p <- mean(my.data[[1]]$prec[sites_top_25, yr])
  nc <- mean(my.data[[1]]$pesticide1[sites_top_25, yr])
  ag <- mean(my.data[[1]]$agriculture[sites_top_25, yr])
  
  one_sp_ev_occ_top <- get.y.val.ns(sims.mat, ss = ss, tt1 = t1, pp = p, nn = nc,  ag = ag, yr = yr, region = region)
  
  one_sp_ev_occ <- bind_rows(mutate(one_sp_ev_occ_bottom, sites = "bottom"), mutate(one_sp_ev_occ_top, sites = "top"))
  
  return(one_sp_ev_occ)
}
