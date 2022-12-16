## get summary function 

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

get.y.val.main <- function(sims.mat, yr) {
  chains <- expit(sims.mat[,'mu.psi.0'] +
                    sims.mat[,'mu.psi.yr']    * yr 
  )
  data.table(data.frame(mean=mean(chains), t(quantile(chains, probs=c(0.025,0.975)))))
}

## main effect for all years

get.y.val.main.all <- function(sims.mat){
  
  all_yr <- unique(my.data[[1]]$yr)
  
  one_sp_ev_occ <- rbindlist(lapply(all_yr, function(x) get.y.val.main(sims.mat = sims.mat, yr = x)))
  
  
  return(bind_cols(one_sp_ev_occ, yr =  all_yr))
}

## species specific main effect 

get.y.val <- function(sims.mat, ss, yr) {
  
  chains <- expit(sims.mat[,'mu.psi.0'] +
                    sims.mat[,sprintf('psi.sp[%d]', ss)]       +
                    sims.mat[,sprintf('psi.era[%s]', ss)]    * yr 
  )
  data.table(data.frame(ss, yr,  mean=mean(chains), t(quantile(chains, probs=c(0.025,0.975)))))
} 


get.y.val.genus <- function(sims.mat, gg, yr, species_directory) {
  
  ss <- filter(species_directory, genus == gg)$sp_n
  
  chains <- expit(sims.mat[,'mu.psi.0'] +
                    sims.mat[,sprintf('psi.sp[%d]', ss)]       +
                    sims.mat[,sprintf('psi.era[%s]', ss)]    * yr 
  )
  
  genus_mean <- rowMeans(chains)
  
  data.table(data.frame(gg, yr, mean=mean(genus_mean), t(quantile(genus_mean, probs=c(0.025,0.975)))))
}
