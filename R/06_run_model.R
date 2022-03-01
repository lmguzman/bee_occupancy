library(stringr)

source("R/src/initialize.R")

run_model <- function(countries, resolution, year_range, model){
  
  ## load data
  if(str_detect(model, 'era')){
    
    all_data <- readRDS(paste0("clean_data/data_prepared/my_data_era_",countries, "_", resolution, "_", paste0(year_range, collapse = "_"), ".rds" ))
    
  }else{
    all_data <- readRDS(paste0("clean_data/data_prepared/my_data_env_",countries, "_", resolution, "_", paste0(year_range, collapse = "_"), ".rds" ))
  }
  
  ## assign data to main vars
  
  my.data <- all_data[[1]]
  nsite <- length(all_data$site)
  nyr <- length(all_data$year)
  nsp <- length(all_data$sp)
  
  ## Initial values 
  Zst <- array(1,dim=c(nsite,nyr,nsp))
  make.inits <- function() {
    RNG <- parallel.seeds("base::BaseRNG", 1)
    c(list(Z=Zst), RNG[[1]])
  }
  inits1 <- make.inits()
  inits2 <- make.inits()
  inits3 <- make.inits()
  
  
  
  ## MCMC settings 
  n.burnin <- 1e2
  n.adapt  <- 1e2
  n.iter   <- 1e4
  n.thin   <- 1e2
  
  ## source JAGS model
  source(sprintf('R/models/%s.R', model))
  model.txt <- sprintf('R/models/%s.txt', model)
  write.model(model.jags, con=model.txt)
  
  res <- run.jags(model=model.txt,
                  monitor=get.params(),
                  data=my.data,
                  inits=list(inits1,inits2,inits3),
                  n.chains=3,
                  burnin=n.burnin,
                  sample=floor(n.iter/n.thin),
                  thin=n.thin,
                  adapt=n.adapt,
                  method='parallel')
  
  saveRDS(res, paste0("model_outputs/res_", countries, "_", resolution, "_", paste0(year_range, collapse = "_"), "_",model, ".rds"))
  
  
  res.summary <-  add.summary(res)
  
  
  saveRDS(res.summary, paste0("model_outputs/res.summary_", countries, "_", resolution, "_", paste0(year_range, collapse = "_"), "_",model, ".rds"))
   
}

run_model('US', 100, c(1996, 2016), 'ms_era_1')



