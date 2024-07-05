## script to check for DAGS ##

library(dagitty)
library(ggdag)
library(ggplot2)
library(tidyr)

### going through DAGs in Figure S4 and testing DAG-data consistency and adjustments

### load and process environmental data 

env_data <- readRDS("clean_data/data_prepared/environment_counties_1995_2015_3.rds")

## pesticide data long

pest_long <- env_data$both_mat_area %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column("site") %>% 
  pivot_longer(values_to = "pesticide", names_to = "year", -site)

## temperature data long

tmax_long <- env_data$tmax %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column("site") %>% 
  pivot_longer(values_to = "tmax", names_to = "year", -site)

## precipitation data long 

prec_long <- env_data$prec %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column("site") %>% 
  pivot_longer(values_to = "prec", names_to = "year", -site)

## agriculture data long

Ag_long <- env_data$ag_mat %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column("site") %>% 
  pivot_longer(values_to = "agriculture", names_to = "year", -site)
 
## Animal pollinated agriculture long

APA_long <- data.frame(APA = env_data$county_fan_mat) %>%
  tibble::rownames_to_column("site")

## honey bee long

honey_long <- env_data$time_col_mat %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column("site") %>% 
  pivot_longer(values_to = "honeybee", names_to = "year", -site)

## join all of the environmental data. 
## we make a dummy occupancy data since it varies by family and needs to account for detection

env_data_long_all <- expand.grid(site = env_data$site_id, year = colnames(env_data$tmax_mat))%>% 
  mutate(occupancy = rbinom(n(),1,  0.5)) %>% 
  left_join(pest_long) %>% 
  left_join(APA_long) %>% 
  left_join(Ag_long) %>% 
  left_join(honey_long) %>% 
  left_join(tmax_long) %>% 
  left_join(prec_long) %>% 
  filter(!is.na(pesticide)) %>% 
  dplyr::select(-site, -year) %>% 
  mutate(climate = tmax + prec)

# DAG A --  Agriculture, Floral resources, nesting resources, pesticide use and occupancy

## build DAG

DAG_A <- dagify(occupancy ~ pesticide + floral + nesting,
                   pesticide ~ agriculture, 
                   nesting ~ agriculture, 
                   floral ~ agriculture,
                   labels = c("pesticide" = "Pesticide",
                              "agriculture" = "Agriculture",
                              "occupancy" = "Occupancy",
                              "floral" = "Floral resources",
                              'nesting' = 'Nesting resources'),
                   latent = c("floral", "nesting"),
                   exposure = "pesticide",
                   outcome = "occupancy"
)


plot(DAG_A)

## test for DAG data consistency

local_tests <- localTests(DAG_A, env_data_long_all, type = 'cis.loess', R=100)

local_tests

impliedConditionalIndependencies(DAG_A)

ggdag::ggdag_adjustment_set(DAG_A,
                            text = FALSE,
                            use_labels  = "label") +
  theme_dag_blank() +
  labs(title = "Adjustment set",
       subtite = "Model for pesticide exposure and occupancy ")

## notes: no implied conditional dependencies, but the dag is too simple and does not account for climate
## adjust for agriculture when estimating the effect of pesticide 


# DAG B --  Animal pollinated agriculture, honey bees, Floral resources, nesting resources, pesticide use and occupancy

## build DAG

DAG_B <- dagify(occupancy ~ pesticide + floral + nesting + honeybee,
                honeybee ~ agriculture,
                pesticide ~ agriculture + honeybee, 
                nesting ~ agriculture, 
                floral ~ agriculture,
                labels = c("pesticide" = "Pesticide",
                           "agriculture" = "Animal Pollinated Agriculture",
                           "occupancy" = "Occupancy",
                           "floral" = "Floral resources",
                           'nesting' = 'Nesting resources',
                           'honeybee' = "Honey Bees"),
                latent = c("floral", "nesting"),
                exposure = "pesticide",
                outcome = "occupancy"
)


plot(DAG_B)

## test for DAG data consistency

impliedConditionalIndependencies(DAG_B)

local_tests <- localTests(DAG_B, env_data_long_all, type = 'cis.loess', R=100)

local_tests

ggdag::ggdag_adjustment_set(DAG_B,
                            text = FALSE,
                            use_labels  = "label") +
  theme_dag_blank() +
  labs(title = "Adjustment set",
       subtite = "Model for pesticide exposure and occupancy ")


## notes: no implied conditional dependencies, but the dag is too simple and does not account for climate
## adjust for agriculture and honey bees when estimating the effect of pesticide 



# DAG C --  Climate, soil and topography, Animal pollinated agriculture, honey bees, Floral resources, nesting resources, pesticide use and occupancy

## build DAG

DAG_C <- dagify(occupancy ~ pesticide + floral + nesting + honeybee + climate,
                honeybee ~ agriculture,
                pesticide ~ agriculture + honeybee, 
                nesting ~ agriculture, 
                floral ~ agriculture + climate,
                agriculture ~ soil + climate,
                labels = c("pesticide" = "Pesticide",
                           "agriculture" = "Animal Pollinated Agriculture",
                           "occupancy" = "Occupancy",
                           "floral" = "Floral resources",
                           'nesting' = 'Nesting resources',
                           'honeybee' = "Honey Bees", 
                           'soil' = 'Soil and Topography', 
                           'climate' = 'Climate'),
                latent = c("floral", "nesting", "soil"),
                exposure = "pesticide",
                outcome = "occupancy"
)


plot(DAG_C)

## test for DAG data consistency

impliedConditionalIndependencies(DAG_C)
#clmt _||_ hnyb | agrc
#clmt _||_ pstc | agrc

env_data_long_all_DC <- env_data_long_all %>% 
  dplyr::select(-agriculture) %>% 
  rename(agriculture = APA)

local_tests <- localTests(DAG_C, env_data_long_all_DC, type = 'cis.loess', R=100)

plotLocalTestResults(local_tests)

ggdag::ggdag_adjustment_set(DAG_C,
                            text = FALSE,
                            use_labels  = "label") +
  theme_dag_blank() +
  labs(title = "Adjustment set",
       subtite = "Model for pesticide exposure and occupancy ")


## notes: local tests fail, pesticide is not independent of climate after controlling for ag, same for honey bees
## adjust for agriculture and honey bees when estimating the effect of pesticide 



# DAG D --  Climate, soil and topography, Animal pollinated agriculture, honey bees, Floral resources, nesting resources, pesticide use and occupancy

## build DAG

DAG_D <- dagify(occupancy ~ pesticide + floral + nesting + honeybee + climate,
                honeybee ~ agriculture,
                pesticide ~ agriculture + honeybee, 
                nesting ~ agriculture, 
                floral ~ agriculture + climate,
                agriculture ~ soil + climate + topography,
                climate ~ topography, 
                labels = c("pesticide" = "Pesticide",
                           "agriculture" = "Animal Pollinated Agriculture",
                           "occupancy" = "Occupancy",
                           "floral" = "Floral resources",
                           'nesting' = 'Nesting resources',
                           'honeybee' = "Honey Bees", 
                           'soil' = 'Soil',
                           'topography' = "Topography",
                           'climate' = 'Climate'),
                latent = c("floral", "nesting", "soil", 'topography'),
                exposure = "pesticide",
                outcome = "occupancy"
)


plot(DAG_D)

## test for DAG data consistency

impliedConditionalIndependencies(DAG_D)
#clmt _||_ hnyb | agrc
#clmt _||_ pstc | agrc

env_data_long_all_DC <- env_data_long_all %>% 
  dplyr::select(-agriculture) %>% 
  rename(agriculture = APA)

local_tests <- localTests(DAG_D, env_data_long_all_DC, type = 'cis.loess', R=100)

plotLocalTestResults(local_tests)

ggdag::ggdag_adjustment_set(DAG_D,
                            text = FALSE,
                            use_labels  = "label") +
  theme_dag_blank() +
  labs(title = "Adjustment set",
       subtite = "Model for pesticide exposure and occupancy ")


## notes: local tests fail (but improve from DAG C), pesticide is not independent of climate after controlling for ag, same for honey bees
## adjust for agriculture and honey bees when estimating the effect of pesticide 



# DAG E --  Climate, soil and topography, Animal pollinated agriculture, honey bees, Floral resources, nesting resources, pesticide use and occupancy

## build DAG

DAG_E <- dagify(occupancy ~ pesticide + floral + nesting + honeybee + climate,
                honeybee ~ agriculture + floral,
                pesticide ~ agriculture + honeybee, 
                nesting ~ agriculture, 
                floral ~ agriculture + climate,
                agriculture ~ soil + climate + topography,
                climate ~ topography, 
                labels = c("pesticide" = "Pesticide",
                           "agriculture" = "Animal Pollinated Agriculture",
                           "occupancy" = "Occupancy",
                           "floral" = "Floral resources",
                           'nesting' = 'Nesting resources',
                           'honeybee' = "Honey Bees", 
                           'soil' = 'Soil',
                           'topography' = "Topography",
                           'climate' = 'Climate'),
                latent = c("floral", "nesting", "soil", 'topography'),
                exposure = "pesticide",
                outcome = "occupancy"
)


plot(DAG_E)

## test for DAG data consistency

impliedConditionalIndependencies(DAG_E)
#clmt _||_ pstc | agrc, hnyb

env_data_long_all_DC <- env_data_long_all %>% 
  dplyr::select(-agriculture) %>% 
  rename(agriculture = APA)

local_tests <- localTests(DAG_E, env_data_long_all_DC, type = 'cis.loess', R=100)

plotLocalTestResults(local_tests)

ggdag::ggdag_adjustment_set(DAG_E,
                            text = FALSE,
                            use_labels  = "label") +
  theme_dag_blank() +
  labs(title = "Adjustment set",
       subtite = "Model for pesticide exposure and occupancy ")


## notes: local tests pass, pesticide IS independent of climate after controlling for ag and honey bees
## adjust for agriculture and honey bees when estimating the effect of pesticide 

## Change exposure sets  -- model 2 agriculture

DAG_E <- dagify(occupancy ~ pesticide + floral + nesting + honeybee + climate,
                honeybee ~ agriculture + floral,
                pesticide ~ agriculture + honeybee, 
                nesting ~ agriculture, 
                floral ~ agriculture + climate,
                agriculture ~ soil + climate + topography,
                climate ~ topography, 
                labels = c("pesticide" = "Pesticide",
                           "agriculture" = "Animal Pollinated Agriculture",
                           "occupancy" = "Occupancy",
                           "floral" = "Floral resources",
                           'nesting' = 'Nesting resources',
                           'honeybee' = "Honey Bees", 
                           'soil' = 'Soil',
                           'topography' = "Topography",
                           'climate' = 'Climate'),
                latent = c("floral", "nesting", "soil", 'topography'),
                exposure = "agriculture",
                outcome = "occupancy"
)

ggdag::ggdag_adjustment_set(DAG_E,
                            text = FALSE,
                            use_labels  = "label") +
  theme_dag_blank() +
  labs(title = "Adjustment set",
       subtite = "Model for pesticide exposure and occupancy ")

## control for Climate

## Change exposure sets  -- model 3 honey bees

DAG_E <- dagify(occupancy ~ pesticide + floral + nesting + honeybee + climate,
                honeybee ~ agriculture + floral,
                pesticide ~ agriculture + honeybee, 
                nesting ~ agriculture, 
                floral ~ agriculture + climate,
                agriculture ~ soil + climate + topography,
                climate ~ topography, 
                labels = c("pesticide" = "Pesticide",
                           "agriculture" = "Animal Pollinated Agriculture",
                           "occupancy" = "Occupancy",
                           "floral" = "Floral resources",
                           'nesting' = 'Nesting resources',
                           'honeybee' = "Honey Bees", 
                           'soil' = 'Soil',
                           'topography' = "Topography",
                           'climate' = 'Climate'),
                latent = c("floral", "nesting", "soil", 'topography'),
                exposure = "honeybee",
                outcome = "occupancy"
)

ggdag::ggdag_adjustment_set(DAG_E,
                            text = FALSE,
                            use_labels  = "label") +
  theme_dag_blank() +
  labs(title = "Adjustment set",
       subtite = "Model for pesticide exposure and occupancy ")

##  Failed to close backdoor paths. Common reasons include:
# graph is not acyclic
# backdoor paths are not closeable with given set of variables
# necessary variables are unmeasured (latent)

## use DAG D for model 3

DAG_D <- dagify(occupancy ~ pesticide + floral + nesting + honeybee + climate,
                honeybee ~ agriculture,
                pesticide ~ agriculture + honeybee, 
                nesting ~ agriculture, 
                floral ~ agriculture + climate,
                agriculture ~ soil + climate + topography,
                climate ~ topography, 
                labels = c("pesticide" = "Pesticide",
                           "agriculture" = "Animal Pollinated Agriculture",
                           "occupancy" = "Occupancy",
                           "floral" = "Floral resources",
                           'nesting' = 'Nesting resources',
                           'honeybee' = "Honey Bees", 
                           'soil' = 'Soil',
                           'topography' = "Topography",
                           'climate' = 'Climate'),
                latent = c("floral", "nesting", "soil", 'topography'),
                exposure = "honeybee",
                outcome = "occupancy"
)

ggdag::ggdag_adjustment_set(DAG_D,
                            text = FALSE,
                            use_labels  = "label") +
  theme_dag_blank() +
  labs(title = "Adjustment set",
       subtite = "Model for pesticide exposure and occupancy ")

## Control for agriculture 