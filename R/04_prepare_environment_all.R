library(dplyr)

source("R/03_prepare_environment_functions.R")

## prepare environment for US 100

prepare_environmental_data("US", 100, scale, c(1996, 2016))
  
## prepare environment for US 50
  
prepare_environmental_data("US", 50, scale, c(1996, 2016))
