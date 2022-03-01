model.jags <- function() {
  
  ### priors
  
  mu.p.0   ~ dnorm(0,0.01)
  
  ## random effect of species on occupancy and detection
  sigma.p.sp   ~ dunif(0,10)
  tau.p.sp    <- 1/(sigma.p.sp*sigma.p.sp)
  for(sp in 1:nsp) {
    p.sp[sp]   ~ dnorm(0, tau.p.sp)
  }
  # 
  
  
  ## random effect of site on detection
  sigma.p.site   ~ dunif(0,10)
  tau.p.site    <- 1/(sigma.p.site*sigma.p.site)
  for(site in 1:nsite) {
    for(yr in 1:nyr) {
      p.site[site,yr]   ~ dnorm(0, tau.p.site)
    }
  }
  
  ## effect of yr on detection
  p.yr ~ dnorm(0,0.01)
  
  
  ## species specific intercepts
  
  ## random effect of species on occupancy and detection
  sigma.psi.sp   ~ dunif(0,10)
  tau.psi.sp    <- 1/(sigma.psi.sp*sigma.psi.sp)
  for(sp in 1:nsp) {
    psi.sp[sp]   ~ dnorm(0, tau.psi.sp)
  }
  # 
  
  
  # main effect of era on occupancy
  
  mu.psi.era ~ dnorm(0,0.01)
  
  mu.psi.0 ~ dnorm(0,0.01)
  
  # species specific slopes on era
  
  sigma.psi.era ~ dunif(0,10)
  tau.psi.era <- 1/(sigma.psi.era*sigma.psi.era)
  
  
  for(sp in 1:nsp){
    psi.era[sp] ~ dnorm(0,tau.psi.era)
  }
  
  # site specific slopes on era
  
  sigma.psi.era.site ~ dunif(0,10)
  tau.psi.era.site <- 1/(sigma.psi.era.site*sigma.psi.era.site)
  
  
  for(site in 1:nsite){
    psi.era.site[site] ~ dnorm(0,tau.psi.era.site)
  }
  
  
  # calcualte occupancy 
  
  for(sp in 1:nsp) {
    for(yr in 1:nyr) {
      for(site in 1:nsite){
        ## occupancy
        logit(psi[site,yr,sp]) <-
          mu.psi.0 +
          psi.sp[sp] +
          mu.psi.era*yr +
          psi.era[sp]*yr +
          psi.era.site[site]*yr
        
        # 
        ## detection
        logit(p[site,yr,sp]) <-
          mu.p.0 +
          p.yr*(yr) +
          p.sp[sp] +
          p.site[site,yr]
        
      }
    }
  }
  
  ## latent state and likelihood    
  
  for(sp in 1:nsp){
    for(site in 1:nsite){
      for(yr in 1:nyr) {
        
        #latent state
        Z[site,yr,sp] ~ dbern(psi[site,yr,sp])
        
      }
      
    }
    
  }
  
  for(ind in 1:nind) {
    mu.p[ind] <-
      Z[site[ind],yr[ind],sp[ind]] *
      p[site[ind],yr[ind],sp[ind]]
    X[ind] ~ dbern(mu.p[ind])
  }
  
}
get.params <- function()
  c('p.yr',
    'mu.p.0',
    'mu.psi.0',
    'mu.psi.era',
    'psi.era',
    'psi.sp', 
    'psi.era.site')














