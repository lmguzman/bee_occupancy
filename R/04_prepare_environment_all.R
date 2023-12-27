## Script to run all functions to arrange the environmental data ##

library(dplyr)

source("R/03_prepare_environment_functions_counties.R")

prepare_environmental_data(c(1995, 2015), 3)

prepare_environmental_data(c(1995, 2014), 2)

prepare_environmental_data(c(1995, 2014), 4)

prepare_environmental_data(c(1995, 2014), 5)


