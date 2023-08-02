
## function to extract model summaries

get.summ <- function(pars, res.summary) {
  summ <- round(cbind(
    res.summary$summary$statistics[pars,'Mean',drop=FALSE],
    res.summary$summary$quantiles[pars,c('2.5%', '97.5%'),drop=FALSE],
    Rhat=res.summary$psrf$psrf[pars,1]
  ), digits=3)
  colnames(summ)[1] <- 'mean'
  summ
}

####### Functions to calculate the mean occupupancy of the family across multiple values of pesiticde (MODEL 1)

## mean effect functions model 1 

get.y.val.main <- function(sims.mat, ps, hb, ag) {
  chains <- expit(sims.mat[,'mu.psi.0'] +
                    sims.mat[,'mu.psi.pest1']    * ps    +
                    sims.mat[,'mu.psi.canag']    * ag    +
                    sims.mat[,'mu.psi.col']* hb
  )
  data.table(data.frame(mean=mean(chains), t(quantile(chains, probs=c(0.025,0.975)))))
}

## mean effect function that wraps around multiple values of pesticide 

get.y.val.main.all <- function(sims.mat, my.data){
    
    hb <- mean(my.data[[1]]$honeybeetime)
    ps <- seq(from=min(my.data[[1]]$pesticidearea),  
              to=  max(my.data[[1]]$pesticidearea),
              length.out=1000)
    ag <- mean(my.data[[1]]$countanimal)
    
    one_sp_ev_occ <- rbindlist(lapply(1:length(ps), function(x) get.y.val.main(sims.mat, hb = hb,  ps = ps[x], ag = ag)))
  
  
  return(bind_cols(one_sp_ev_occ, ps = ps, ag = ag, hb = hb))
}


########## Functions to calculate the genus trends #######

## genus function pesticide Model 1 ####

get.y.val.genus <- function(sims.mat, ss, gg, ps, hb, ag) {
  
  chains <- expit(sims.mat[,'mu.psi.0'] +
                    sims.mat[,sprintf('psi.sp[%d]', ss)]       +
                    sims.mat[,sprintf('psi.pest1[%s]', ss)]    * ps +
                    sims.mat[,sprintf('psi.col[%s]', ss)]     * hb +
                    sims.mat[,sprintf('psi.canag[%s]', ss)]     * ag
                  
  )
  
  genus_mean <- rowMeans(chains)
  
  data.table(data.frame(gg, ps, hb, ag, mean=mean(genus_mean), t(quantile(genus_mean, probs=c(0.025,0.975)))))
}

get.y.val.genus.all <- function(sims.mat, my.data, species_directory){
  
  hb <- mean(my.data[[1]]$honeybeetime)
  ps <- seq(from=min(my.data[[1]]$pesticidearea),  
            to=  max(my.data[[1]]$pesticidearea),
            length.out=1000)
  ag <- mean(my.data[[1]]$countanimal)

  one_sp_ev_occ <- list()
  
  for(gg in unique(species_directory$genus)){
    ss <- filter(species_directory, genus == gg)$sp_n
    
    one_sp_ev_occ[[gg]] <- rbindlist(lapply(1:length(ps), function(x) get.y.val.genus(sims.mat, gg = gg, ss = ss, hb = hb,  ps = ps[x], ag = ag)))
    
  }
  
  
  return(rbindlist(one_sp_ev_occ))
}


######## get genus animal pollinated agriculture ########

## genus function apa (Model 2)  ####

get.y.val.genus.canag <- function(sims.mat, ss, gg, ag, tt, pc) {
  
  chains <- expit(sims.mat[,'mu.psi.0'] +
                    sims.mat[,sprintf('psi.sp[%d]', ss)]       +
                    sims.mat[,sprintf('psi.tmax1[%s]', ss)]    * tt +
                    sims.mat[,sprintf('psi.tmax2[%s]', ss)]    * tt^2 +
                    sims.mat[,sprintf('psi.prec[%s]', ss)]     * pc +
                    sims.mat[,sprintf('psi.canag[%s]', ss)]     * ag
                  
  )
  
  genus_mean <- rowMeans(chains)
  
  data.table(data.frame(gg, ag, tt, pc, mean=mean(genus_mean), t(quantile(genus_mean, probs=c(0.025,0.975)))))
}

get.y.val.genus.all.canag <- function(sims.mat, my.data, species_directory){
  
  pc <- mean(my.data[[1]]$prec)
  tt <- mean(my.data[[1]]$tmax)
  ag <- seq(from=min(my.data[[1]]$countanimal),  
            to=  max(my.data[[1]]$countanimal),
            length.out=1000)
  
  one_sp_ev_occ <- list()
  
  for(gg in unique(species_directory$genus)){
    ss <- filter(species_directory, genus == gg)$sp_n
    
    one_sp_ev_occ[[gg]] <- rbindlist(lapply(1:length(ag), function(x) get.y.val.genus.canag(sims.mat, gg = gg, ss = ss, tt = tt, pc = pc, ag = ag[x])))
    
  }
  
  
  return(rbindlist(one_sp_ev_occ))
}
