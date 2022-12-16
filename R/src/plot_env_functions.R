
get.summ <- function(pars, res.summary) {
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

get.y.val.main.all <- function(sims.mat, ev, my.data){
  
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

### species specific function 

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


## genus function ####

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


## calculating occupancy for all species for a certain environment ###

get.y.val.all <- function(sims.mat, ev, ss, my.data){
  
  if(ev == "temp"){
    
    t1 <- seq(from=min(my.data[[1]]$tmax),  
              to=  max(my.data[[1]]$tmax),
              length.out=1000)
    p <- mean(my.data[[1]]$prec)
    nc <- mean(my.data[[1]]$pesticide1)
    ag <- mean(my.data[[1]]$agriculture)
    
    one_sp_ev_occ <- rbindlist(lapply(1:length(t1), function(x) get.y.val(sims.mat, ss = ss, tt1 = t1[x],
                                                                          pp = p, nn = nc, ag = ag)))
    
    
  }else if(ev == 'prec'){
    
    t1 <- mean(my.data[[1]]$tmax)
    p <- seq(from=min(my.data[[1]]$prec),  
             to=  max(my.data[[1]]$prec),
             length.out=1000)
    nc <- mean(my.data[[1]]$pesticide1)
    ag <- mean(my.data[[1]]$agriculture)
    
    one_sp_ev_occ <- rbindlist(lapply(1:length(p), function(x) get.y.val(sims.mat, ss = ss, tt1 = t1,
                                                                         pp = p[x],  nn = nc, ag = ag)))
    
  }else if(ev == 'neonic'){
    
    t1 <- mean(my.data[[1]]$tmax)
    p <- mean(my.data[[1]]$prec)
    nc <- seq(from=min(my.data[[1]]$pesticide1),  
              to=  max(my.data[[1]]$pesticide1),
              length.out=1000)
    ag <- mean(my.data[[1]]$agriculture)
    
    one_sp_ev_occ <- rbindlist(lapply(1:length(nc), function(x) get.y.val(sims.mat, ss = ss, tt1 = t1, 
                                                                          pp = p, nn = nc[x], ag = ag)))
    
    
  }else if(ev == 'agriculture'){
    
    t1 <- mean(my.data[[1]]$tmax)
    p <- mean(my.data[[1]]$prec)
    nc <- mean(my.data[[1]]$pesticide1)
    ag <- seq(from=min(my.data[[1]]$agriculture),  
              to=  max(my.data[[1]]$agriculture),
              length.out=1000)
    
    one_sp_ev_occ <- rbindlist(lapply(1:length(ag), function(x) get.y.val(sims.mat, ss = ss, tt1 = t1, 
                                                                          pp = p, nn = nc,  ag = ag[x])))
    
  }
  
  return(one_sp_ev_occ)
}



### get genus occupancy for any environmental variation ##

get.y.val.all.genus <- function(sims.mat, ev, gg, my.data, species_directory){
  
  ss <- filter(species_directory, genus == gg)$sp_n
  
  if(ev == "temp"){
    t1 <- seq(from=min(my.data[[1]]$tmax),  
              to=  max(my.data[[1]]$tmax),
              length.out=1000)
    p <- mean(my.data[[1]]$prec)
    nc <- mean(my.data[[1]]$pesticide1)
    ag <- mean(my.data[[1]]$agriculture)
    
    one_sp_ev_occ <- rbindlist(lapply(1:length(t1), function(x) get.y.val.genus(sims.mat, gg = gg, ss = ss, tt1 = t1[x], 
                                                                                pp = p, nn = nc, ag = ag)))
    
    
  }else if(ev == 'prec'){
    
    t1 <- mean(my.data[[1]]$tmax)
    p <- seq(from=min(my.data[[1]]$prec),  
             to=  max(my.data[[1]]$prec),
             length.out=1000)
    nc <- mean(my.data[[1]]$pesticide1)
    ag <- mean(my.data[[1]]$agriculture)
    
    one_sp_ev_occ <- rbindlist(lapply(1:length(p), function(x) get.y.val.genus(sims.mat, gg = gg, ss = ss, tt1 = t1, 
                                                                               pp = p[x],  nn = nc,  ag = ag)))
    
  }else if(ev == 'neonic'){
    
    t1 <- mean(my.data[[1]]$tmax)
    p <- mean(my.data[[1]]$prec)
    nc <- seq(from=min(my.data[[1]]$pesticide1),  
              to=  max(my.data[[1]]$pesticide1),
              length.out=1000)
    ag <- mean(my.data[[1]]$agriculture)
    
    one_sp_ev_occ <- rbindlist(lapply(1:length(nc), function(x) get.y.val.genus(sims.mat, gg = gg, ss = ss, tt1 = t1, 
                                                                                pp = p, nn = nc[x], ag = ag)))
    
  }else if(ev == 'agriculture'){
    
    t1 <- mean(my.data[[1]]$tmax)
    p <- mean(my.data[[1]]$prec)
    nc <- mean(my.data[[1]]$pesticide1)
    ag <- seq(from=min(my.data[[1]]$agriculture),  
              to=  max(my.data[[1]]$agriculture),
              length.out=1000)
    
    one_sp_ev_occ <- rbindlist(lapply(1:length(ag), function(x) get.y.val.genus(sims.mat, gg = gg, ss = ss, tt1 = t1,
                                                                                pp = p, nn = nc,  ag = ag[x])))
    
  }
  
  return(one_sp_ev_occ)
}

