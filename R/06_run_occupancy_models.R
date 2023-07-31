####### Script to run occupancy models ###### 
## note, the short version of the for loop takes about ~24 hrs
## the long version can take about a week
## you will need at least 45 GB of RAM memory to run the for loop

library(stringr)

source("R/src/initialize.R")

run_model <- function(model, family, run_len){
  
  ## load data
  all_data <- readRDS(paste0("clean_data/data_prepared/my_data_env_genus_filtered_trait_agriregion_both_pest_area_county_1995_2015_", family, "_ALLFALSE.rds" ))
  
  ### assign data to main vars
  
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
  
  if(run_len == 'short'){
    n.burnin <- 1e2
    n.adapt  <- 1e2
    n.iter   <- 1e4
    n.thin   <- 1e2
  }else if(run_len == 'long'){
    n.burnin <- 1e3
    n.adapt  <- 1e3
    n.iter   <- 1e5
    n.thin   <- 1e3
  }
  
  
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
  
  saveRDS(res, paste0("model_outputs/res_genus_filtered_agriregion_pest_area_county_both_1995_2015_",model,"_", family, "_ALLFALSE.rds"))
  
  ## add summary 
  
  res.summary <-  add.summary(res)
  
  
  saveRDS(res.summary, paste0("model_outputs/res.summary_genus_filtered_agriregion_pest_area_county_both_1995_2015_",model,"_", family, "_ALLFALSE.rds"))
  
}


fam <- c("Andrenidae", "Apidae", "Halictidae", 
         "Megachilidae", "Colletidae|Melittidae")

### using main text animal pollinated agriculture

#### run Model (1) for pesticide 

for(f in fam){
  run_model('ms_area_honeytime_pestar_canag', f, 'short')
}

#### run Model (2) for honey bees

for(f in fam){
  run_model('ms_area_honeytime_canag', f, 'short')
}

#### run Model (3) for animal pollinated agriculture

for(f in fam){
  run_model('ms_area_climate_canag', f, 'short')
}


### changing the data so it only uses crops that use managed pollinators

#### run Model (1) for pesticide

for(f in fam){
  run_model('ms_area_honeytime_pestar_canagmb', f, 'short')
}

#### run Model (2) for honey bees 

for(f in fam){
  run_model('ms_area_honeytime_canagmb', f, 'short')
}

#### run Model (3) for animal pollinated agriculture 

for(f in fam){
  run_model('ms_area_climate_canagmb', f, 'short')
}


### changing the data so it only uses crops that attract pollinators

#### run Model (1) for pesticide

for(f in fam){
  run_model('ms_area_honeytime_pestar_canagabs', f, 'short')
}

#### run Model (2) for honey bees 

for(f in fam){
  run_model('ms_area_honeytime_canagabs', f, 'short')
}

#### run Model (3) for animal pollinated agriculture 

for(f in fam){
  run_model('ms_area_climate_canagabs', f, 'short')
}
