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
  p.era ~ dnorm(0,0.01)
  
  # random effect of year on initial colonization and extinction
  
  sigma.psi.sp ~ dunif(0,10)
  tau.psi.sp <- 1/(sigma.psi.sp*sigma.psi.sp)
  
  mu.psi.0 ~ dnorm(0,0.01)
  
  for(sp in 1:nsp){
    psi.sp[sp] ~ dnorm(0,tau.psi.sp)
  }
  
  ## fraction of animal pollinated agriculture
  
  mu.psi.fanag ~ dnorm(0,0.01)
  sigma.psi.fanag ~ dunif(0,10)
  tau.psi.fanag <- 1/(sigma.psi.fanag*sigma.psi.fanag)
  
  for(sp in 1:nsp){
    psi.fanag[sp] ~ dnorm(mu.psi.fanag, tau.psi.fanag)
  }
  
  ## Interaction between fan and ag
  
  mu.psi.int ~ dnorm(0,0.01)
  sigma.psi.int ~ dunif(0,10)
  tau.psi.int <- 1/(sigma.psi.int*sigma.psi.int)
  
  for(sp in 1:nsp){
    psi.int[sp] ~ dnorm(mu.psi.int, tau.psi.int)
  }
  

  ## Species specific slopes to each type of pesticide
  
  mu.psi.pest1 ~ dnorm(0,0.01)
  sigma.psi.pest1 ~ dunif(0,10)
  tau.psi.pest1 <- 1/(sigma.psi.pest1*sigma.psi.pest1)
  
  for(sp in 1:nsp){
    psi.pest1[sp] ~ dnorm(mu.psi.pest1, tau.psi.pest1)
  }
  
  
  ## agriculture
  
  mu.psi.agric ~ dnorm(0,0.01)
  sigma.psi.agric ~ dunif(0,10)
  tau.psi.agric <- 1/(sigma.psi.agric*sigma.psi.agric)
  
  for(sp in 1:nsp){
    psi.agric[sp] ~ dnorm(mu.psi.agric, tau.psi.agric)
  }
  
  # area
  
  psi.area ~ dnorm(0,0.01)
  
  #####
  
  for(sp in 1:nsp) {
    for(yr in 1:nyr) {
      for(site in 1:nsite){
        ## occupancy
        logit(psi[site,yr,sp]) <-
          mu.psi.0 +
          psi.sp[sp] +
          psi.area*area[site] +
          psi.fanag[sp]*fracanimal[site] +           
          psi.agric[sp]*agriculture[site,yr] +
          psi.int[sp]*fracanimal[site]*agriculture[site,yr] +
          psi.pest1[sp]*pesticide1[site,yr] 
        
        # 
        ## detection
        logit(p[site,yr,sp]) <-
          mu.p.0 +
          p.era*yr +
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
  c('p.era',
    'mu.p.0',
    'mu.psi.0',
    'mu.psi.pest1',
    'mu.psi.agric',
    'mu.psi.fanag',
    'mu.psi.int',
    'psi.sp',
    'psi.pest1',
    'psi.agric',
    'psi.fanag',
    'psi.int')