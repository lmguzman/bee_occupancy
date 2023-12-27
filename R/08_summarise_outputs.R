#### this is a script to summarise the model outputs ###

library(dplyr)
library(ggplot2)
library(purrr)
library(cowplot)
library(stringr)
library(viridis)
source("R/src/initialize.R")

## function to extract model summaries 

get.summ <- function(pars) {
  summ <- round(cbind(
    res.summary$summary$statistics[pars,'Mean',drop=FALSE],
    res.summary$summary$quantiles[pars,c('2.5%', '97.5%'),drop=FALSE],
    Rhat=res.summary$psrf$psrf[pars,1]
  ), digits=3)
  colnames(summ)[1] <- 'mean'
  summ
}

## list all model outputs
files_results <- list.files("model_outputs/")

## get only model outputs with filtered genus

unique_results <- files_results[str_detect(files_results, "filtered_agriregion_pest_area_county")]

## create a list where we can compile results 
compiled_results <- list()

## for each file: 
for(f in unique_results){
  
  # load file
  res.summary <- readRDS(paste0("model_outputs/",f)) 

  run_length <- 'short'
  
  if(res.summary$thin == 1000){
     run_length <- "long"
  }
  
  # extrat the mean effects
  vars <- rownames(res.summary$psrf$psrf)
  summ <- get.summ(vars)
  
  summ.paper <- summ[str_detect(rownames(summ), 'mu.psi'),]
  
  ## extract the model type 
  
  model <- str_extract(f, "ms_area_honeytime_pestar_canag|ms_area_climate_canag|ms_area_honeytime_canag")
  
  ## type of ag
  
  model_type_ag <- str_extract(f, "canagabs|canagmb|canag")
  
  ## extract the family
  family <- str_extract(f, "Andrenidae|Apidae|Colletidae|Halictidae|Megachilidae")
  
  # get the variable name
  variable <- str_remove(rownames(summ.paper), "mu.psi.")
  
  if(nrow(summ.paper) == 7){
    compiled_results[[f]] <- data.frame(summ.paper, model = model, family = family, model_type_ag = model_type_ag,
                                        variable = variable, run_length = run_length)
    
  }else{
    compiled_results[[f]] <- data.frame(summ.paper, model = model, family = family, model_type_ag = model_type_ag, 
                                        run_length = run_length, 
                                        variable = variable) 
  }

}

all_results <- compiled_results %>% 
  map_df(~as.data.frame(.x), .id = "file_name") %>% 
  mutate(family = ifelse(family == 'Colletidae', 'Colletidae|Melittidae', family))

saveRDS(all_results, "model_outputs/all_results.rds")









