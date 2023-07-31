
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
  
  ## county of animal pollinated agriculture

## county of animal pollinated agriculture
  
  mu.psi.canag ~ dnorm(0,0.01)
  sigma.psi.canag ~ dunif(0,10)
  tau.psi.canag <- 1/(sigma.psi.canag*sigma.psi.canag)
  
  for(sp in 1:nsp){
    psi.canag[sp] ~ dnorm(mu.psi.canag, tau.psi.canag)
  }

 ### honey bee colonies 
 
  mu.psi.col ~ dnorm(0,0.01)
  sigma.psi.col ~ dunif(0,10)
  tau.psi.col <- 1/(sigma.psi.col*sigma.psi.col)
  
  for(sp in 1:nsp){
    psi.col[sp] ~ dnorm(mu.psi.col, tau.psi.col)
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
          psi.col[sp]*honeybeetime[site,yr] +           
          psi.canag[sp]*countanimal[site] 
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
    'mu.psi.col',
    'mu.psi.canag',
    'psi.sp',
    'psi.col',
    'psi.canag',
    'psi.area')
