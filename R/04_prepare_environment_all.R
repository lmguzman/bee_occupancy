library(dplyr)

source("R/03_2_prepare_environment_functions_counties.R")

prepare_environmental_data(scale, c(1995, 2015))



## old
## prepare environment for US 100

prepare_environmental_data("US", 100, scale, c(1997, 2016))
  
prepare_environmental_data("US", 100, scale, c(1996, 2016))

## prepare environment for US 50
  
prepare_environmental_data("US", 50, scale, c(1997, 2016))
