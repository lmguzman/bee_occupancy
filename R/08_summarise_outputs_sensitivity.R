#### this is a script to summarise the model outputs ###

library(dplyr)
library(ggplot2)
library(purrr)
library(cowplot)
library(stringr)
library(viridis)
library(forcats)
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
files_results <- list.files("model_outputs/sensitivity_oc/")

## get only model outputs with filtered genus

unique_results <- files_results[str_detect(files_results, "filtered_agriregion_pest_area_county")]

## create a list where we can compile results 
compiled_results <- list()

## for each file: 
for(f in unique_results){
  
  # load file
  res.summary <- readRDS(paste0("model_outputs/sensitivity_oc/",f)) 
  
  run_length <- 'short'
  
  if(res.summary$thin == 1000){
    run_length <- "long"
  }
  
  # extrat the mean effects
  vars <- rownames(res.summary$psrf$psrf)
  summ <- get.summ(vars)
  
  summ.paper <- summ[str_detect(rownames(summ), 'mu.psi'),]
  
  ## extract the model type 
  model <- str_extract(f, 'ms_area_honeytime_pestar_canag|ms_area_honeytime_canag|ms_area_climate_canag')
  
  ## extract the family
  family <- str_remove(str_remove(str_extract(f, "ms_\\S*_ALL"), 'ms_area_honeytime_pestar_canag_|ms_area_honeytime_canag_|ms_area_climate_canag_'), "_ALL")
  
  # get the variable name
  variable <- str_remove(rownames(summ.paper), "mu.psi.")
  
  ## get the occupancy interval 
  year_oc <- str_extract(f, "\\d+_\\d+")
  
  year <- str_sub(year_oc, end = -2)
  
  oc_int <- str_sub(year_oc, start = 10)
  
  if(nrow(summ.paper) == 7){
    compiled_results[[f]] <- data.frame(summ.paper, model = model, family = family, 
                                        variable = variable, run_length = run_length,
                                        year = year, oc_int = oc_int)
    
  }else{
    compiled_results[[f]] <- data.frame(summ.paper, model = model, family = family, 
                                        run_length = run_length, 
                                        variable = variable,
                                        year = year, oc_int = oc_int) 
  }
  
}

all_results <- compiled_results %>% 
  map_df(~as.data.frame(.x), .id = "file_name")

saveRDS(all_results, "model_outputs/all_results_sensitivity.rds")

### plot sensitivity ## 


var_names_m1 <- data.frame(variable = c('0', 'canag', 'col', 
                                     'pest1'), 
                        variable_nice = c('Intercept', "Animal Pollinated\n Agriculture", 
                                          "Honey Bees", 
                                          "Pesticide")) 

sensitivity_m1 <- all_results %>% 
  filter(model == 'ms_area_honeytime_pestar_canag') %>% 
  left_join(var_names_m1) %>% 
  mutate(variable_nice = fct_relevel(variable_nice, 'Intercept', "Pesticide", 
                                     "Animal Pollinated\n Agriculture", 
                                     "Honey Bees")) %>% 
  ggplot() +
  geom_point(aes(x = mean, y = oc_int)) +
  geom_errorbarh(aes(y = oc_int, xmin = `X2.5.`, xmax = X97.5.), height = 0) +
  facet_grid(family ~variable_nice, scales = 'free_x') +
  geom_vline(xintercept = 0, linetype = "dashed", colour = 'grey') +
  theme_cowplot() +
  xlab("Estimated effect") +
  ylab("Length of occupancy interval in years") +
  theme(strip.background = element_blank())


ggsave(sensitivity_m1, filename = 'plots/sensitivity_model1.pdf', height = 9, width = 10)



var_names_m3 <- data.frame(variable = c('0', 'canag', 'col'), 
                           variable_nice = c('Intercept', "Animal Pollinated\n Agriculture", 
                                             "Honey Bees")) 

sensitivity_m3 <- all_results %>% 
  filter(model == 'ms_area_honeytime_canag') %>% 
  left_join(var_names_m3) %>% 
  mutate(variable_nice = fct_relevel(variable_nice, 'Intercept', 
                                     "Animal Pollinated\n Agriculture", 
                                     "Honey Bees")) %>% 
  ggplot() +
  geom_point(aes(x = mean, y = oc_int)) +
  geom_errorbarh(aes(y = oc_int, xmin = `X2.5.`, xmax = X97.5.), height = 0) +
  facet_grid(family ~variable_nice, scales = 'free_x') +
  geom_vline(xintercept = 0, linetype = "dashed", colour = 'grey') +
  theme_cowplot() +
  xlab("Estimated effect") +
  ylab("Length of occupancy interval") +
  theme(strip.background = element_blank())

ggsave(sensitivity_m3, filename = 'plots/sensitivity_model3.pdf', height = 9, width = 10)




var_names_m2 <- data.frame(variable = c('0', 'canag', 'tmax1', 'tmax2', 'prec'), 
                           variable_nice = c('Intercept', "Animal Pollinated\n Agriculture", 
                                             "Temperature", "Temperature (quadratic)", "Precipitation")) 

sensitivity_m2 <- all_results %>% 
  filter(model == 'ms_area_climate_canag') %>% 
  left_join(var_names_m2) %>% 
  mutate(variable_nice = fct_relevel(variable_nice, 'Intercept', "Animal Pollinated\n Agriculture", 
                                     "Temperature", "Temperature (quadratic)", "Precipitation")) %>% 
  ggplot() +
  geom_point(aes(x = mean, y = oc_int)) +
  geom_errorbarh(aes(y = oc_int, xmin = `X2.5.`, xmax = X97.5.), height = 0) +
  facet_grid(family ~variable_nice, scales = 'free_x') +
  geom_vline(xintercept = 0, linetype = "dashed", colour = 'grey') +
  theme_cowplot() +
  xlab("Estimated effect") +
  ylab("Length of occupancy interval") +
  theme(strip.background = element_blank())

ggsave(sensitivity_m2, filename = 'plots/sensitivity_model2.pdf', height = 9, width = 10)





