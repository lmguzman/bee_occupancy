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
  
  
  ## Interaction between fan and ag
  
  mu.psi.int.ag.fan ~ dnorm(0,0.01)
  sigma.psi.int.ag.fan ~ dunif(0,10)
  tau.psi.int.ag.fan <- 1/(sigma.psi.int.ag.fan*sigma.psi.int.ag.fan)
  
  for(sp in 1:nsp){
    psi.int.ag.fan[sp] ~ dnorm(mu.psi.int.ag.fan, tau.psi.int.ag.fan)
  }
  
  ## Interaction between pest and ag
  
  mu.psi.int.ag.pest ~ dnorm(0,0.01)
  sigma.psi.int.ag.pest ~ dunif(0,10)
  tau.psi.int.ag.pest <- 1/(sigma.psi.int.ag.pest*sigma.psi.int.ag.pest)
  
  for(sp in 1:nsp){
    psi.int.ag.pest[sp] ~ dnorm(mu.psi.int.ag.pest, tau.psi.int.ag.pest)
  }
  
  ## Interaction between pest and fan
  
  mu.psi.int.pest.fan ~ dnorm(0,0.01)
  sigma.psi.int.pest.fan ~ dunif(0,10)
  tau.psi.int.pest.fan <- 1/(sigma.psi.int.pest.fan*sigma.psi.int.pest.fan)
  
  for(sp in 1:nsp){
    psi.int.pest.fan[sp] ~ dnorm(mu.psi.int.pest.fan, tau.psi.int.pest.fan)
  }
  
  ## Interaction between pest and fan and ag
  
  mu.psi.int.pest.fan.ag ~ dnorm(0,0.01)
  sigma.psi.int.pest.fan.ag ~ dunif(0,10)
  tau.psi.int.pest.fan.ag <- 1/(sigma.psi.int.pest.fan.ag*sigma.psi.int.pest.fan.ag)
  
  for(sp in 1:nsp){
    psi.int.pest.fan.ag[sp] ~ dnorm(mu.psi.int.pest.fan.ag, tau.psi.int.pest.fan.ag)
  }
  
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
          psi.pest1[sp]*pesticide1[site,yr] +
          psi.int.ag.fan[sp]*fracanimal[site]*agriculture[site,yr] +
          psi.int.ag.pest[sp]*pesticide1[site,yr]*agriculture[site,yr] +
          psi.int.pest.fan[sp]*fracanimal[site]*pesticide1[site,yr] +
          psi.int.pest.fan.ag[sp]*fracanimal[site]*pesticide1[site,yr] *agriculture[site,yr]
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
    'mu.psi.int.ag.fan',
    'mu.psi.int.ag.pest',
    'mu.psi.int.pest.fan',
    'mu.psi.int.pest.fan.ag',
    'psi.sp',
    'psi.pest1',
    'psi.agric',
    'psi.fanag',
    'psi.int.ag.fan',
    'psi.int.ag.pest',
    'psi.int.pest.fan',
    'psi.int.pest.fan.ag')