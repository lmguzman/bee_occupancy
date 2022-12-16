
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




## calculating occupancy for all species for a certain environment ###

get.y.val.all <- function(sims.mat, ss, my.data, yr, ranges, species_directory, region = region){
    

    sites_sp <- my.data[[3]][[filter(species_directory, sp_n == ss)$finalName]]
  
    sites_in_region <- rownames(my.data[[1]]$tmax)
    
    species_sites_in_region <- sites_sp[sites_sp %in% sites_in_region]
  
    t1 <- mean(my.data[[1]]$tmax[sites_in_region, yr])
    p <- mean(my.data[[1]]$prec[sites_in_region, yr])
    nc <- mean(my.data[[1]]$pesticide1[sites_in_region, yr])
    ag <- mean(my.data[[1]]$agriculture[sites_in_region, yr])
    
    one_sp_ev_occ <- get.y.val(sims.mat, ss = ss, tt1 = t1, pp = p, nn = nc,  ag = ag, yr = yr, region = region)
    

  
  return(one_sp_ev_occ)
}





get.y.val.all.neonic <- function(sims.mat, ss, my.data, yr, ranges, species_directory, region = region){
  
  ## Varying only neonics
  
  sites_sp <- my.data[[3]][[filter(species_directory, sp_n == ss)$finalName]]
  
  sites_in_region <- rownames(my.data[[1]]$tmax)
  
  species_sites_in_region <- sites_sp[sites_sp %in% sites_in_region]
  
  t1 <- mean(my.data[[1]]$tmax[sites_in_region, yr])
  p <- mean(my.data[[1]]$prec[sites_in_region, yr])
  nc <- min(my.data[[1]]$pesticide1)
  ag <- mean(my.data[[1]]$agriculture[sites_in_region, yr])
  
  one_sp_ev_occ <- get.y.val(sims.mat, ss = ss, tt1 = t1, pp = p, nn = nc,  ag = ag, yr = yr, region = region)
  
  
  
  return(one_sp_ev_occ)
}
