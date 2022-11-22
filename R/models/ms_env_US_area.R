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
  
  
  
  # random effect of year on initial colonization and extinction
  
  sigma.psi.sp ~ dunif(0,10)
  tau.psi.sp <- 1/(sigma.psi.sp*sigma.psi.sp)
  
  mu.psi.0 ~ dnorm(0,0.01)
  
  for(sp in 1:nsp){
    psi.sp[sp] ~ dnorm(0,tau.psi.sp)
  }
  
  ## Species specific slopes to temperature
  
  mu.psi.tmax1 ~ dnorm(0,0.01)
  sigma.psi.tmax1 ~ dunif(0,10)
  tau.psi.tmax1 <- 1/(sigma.psi.tmax1*sigma.psi.tmax1)
  
  for(sp in 1:nsp){
    psi.tmax1[sp] ~ dnorm(mu.psi.tmax1, tau.psi.tmax1)
  }
  
  mu.psi.tmax2 ~ dnorm(0,0.01)
  sigma.psi.tmax2 ~ dunif(0,10)
  tau.psi.tmax2 <- 1/(sigma.psi.tmax2*sigma.psi.tmax2)
  
  for(sp in 1:nsp){
    #psi.tmax2[sp] ~ dnorm(mu.psi.tmax2, tau.psi.tmax2);T(,0)
    psi.tmax2[sp] ~ dnorm(mu.psi.tmax2, tau.psi.tmax2)
  }
  
  
  ## Species specific slopes to precipitation
  
  mu.psi.prec ~ dnorm(0,0.01)
  sigma.psi.prec ~ dunif(0,10)
  tau.psi.prec <- 1/(sigma.psi.prec*sigma.psi.prec)
  
  for(sp in 1:nsp){
    psi.prec[sp] ~ dnorm(mu.psi.prec, tau.psi.prec)
  }
  
  ## Species specific slopes to each type of pesticide
  
  mu.psi.pest1 ~ dnorm(0,0.01)
  sigma.psi.pest1 ~ dunif(0,10)
  tau.psi.pest1 <- 1/(sigma.psi.pest1*sigma.psi.pest1)
  
  for(sp in 1:nsp){
    psi.pest1[sp] ~ dnorm(mu.psi.pest1, tau.psi.pest1)
  }
  
  mu.psi.pest2 ~ dnorm(0,0.01)
  sigma.psi.pest2 ~ dunif(0,10)
  tau.psi.pest2 <- 1/(sigma.psi.pest2*sigma.psi.pest2)
  
  for(sp in 1:nsp){
    psi.pest2[sp] ~ dnorm(mu.psi.pest2, tau.psi.pest2)
  }
  
  mu.psi.pest3 ~ dnorm(0,0.01)
  sigma.psi.pest3 ~ dunif(0,10)
  tau.psi.pest3 <- 1/(sigma.psi.pest3*sigma.psi.pest3)
  
  for(sp in 1:nsp){
    psi.pest3[sp] ~ dnorm(mu.psi.pest3, tau.psi.pest3)
  }
  
  
  ## agriculture
  
  mu.psi.agric ~ dnorm(0,0.01)
  sigma.psi.agric ~ dunif(0,10)
  tau.psi.agric <- 1/(sigma.psi.agric*sigma.psi.agric)
  
  for(sp in 1:nsp){
    psi.agric[sp] ~ dnorm(mu.psi.agric, tau.psi.agric)
  }
  
  ## drought
  
  mu.psi.drought ~ dnorm(0,0.01)
  sigma.psi.drought ~ dunif(0,10)
  tau.psi.drought <- 1/(sigma.psi.drought*sigma.psi.drought)
  
  for(sp in 1:nsp){
    psi.drought[sp] ~ dnorm(mu.psi.drought, tau.psi.drought)
  }
  

  ## floral
  
  mu.psi.floral ~ dnorm(0,0.01)
  sigma.psi.floral ~ dunif(0,10)
  tau.psi.floral <- 1/(sigma.psi.floral*sigma.psi.floral)
  
  for(sp in 1:nsp){
    psi.floral[sp] ~ dnorm(mu.psi.floral, tau.psi.floral)
  }
  
  
  ## nest
  
  mu.psi.nest ~ dnorm(0,0.01)
  sigma.psi.nest ~ dunif(0,10)
  tau.psi.nest <- 1/(sigma.psi.nest*sigma.psi.nest)
  
  for(sp in 1:nsp){
    psi.nest[sp] ~ dnorm(mu.psi.nest, tau.psi.nest)
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
          psi.tmax1[sp]*tmax[site,yr] + ## warm sites have higher persistence (+)
          psi.tmax2[sp]*tmax[site,yr]^2 +
          psi.prec[sp]*prec[site,yr] + 
          psi.pest1[sp]*pesticide1[site,yr] +
          psi.pest2[sp]*pesticide2[site,yr] +
          psi.pest3[sp]*pesticide3[site,yr] +
          psi.agric[sp]*agriculture[site,yr] +
          psi.drought[sp]*drought[site,yr]+
          psi.floral[sp]*floral[site,yr]+
          psi.nest[sp]*nesting[site,yr]
        
        # 
        ## detection
        logit(p[site,yr,sp]) <-
          mu.p.0 +
          p.yr*(yr-1) +
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
    'mu.psi.tmax1',
    'mu.psi.tmax2',
    'mu.psi.prec',
    'mu.psi.pest1',
    'mu.psi.pest2',
    'mu.psi.pest3',
    'mu.psi.agric',
    'mu.psi.drought',
    'mu.psi.floral',
    'mu.psi.nest',
    'psi.sp',
    'psi.tmax1',
    'psi.tmax2',
    'psi.prec', 
    'psi.pest1',
    'psi.pest2',
    'psi.pest3',
    'psi.agric',
    'psi.drought',
    'psi.floral',
    'psi.nest')














