## script to check for DAGS ##

library(dagitty)
library(ggdag)
library(ggplot2)


### NAPA -- non pollinated agriculture ### 

# build graph
napa_dag <- dagify(occupancy ~ pesticide + floral + nesting,
                     pesticide ~ NAPA, 
                     nesting ~ NAPA, 
                     floral ~ NAPA,
                     labels = c("pesticide" = "Pesticide",
                                "NAPA" = "Non- animal pollinated agriculture",
                                "occupancy" = "Occupancy",
                                "floral" = "Floral resources",
                                'nesting' = 'Nesting resources'),
                     latent = c("floral", "nesting"),
                     exposure = "pesticide",
                     outcome = "occupancy"
)


plot(napa_dag)


### APA -- animal pollinated agriculture ### 

#build graph

apa_dag <- dagify(occupancy ~ pesticide + floral + nesting + honeybee,
                     pesticide ~ APA + honeybee, 
                     nesting ~ APA, 
                     floral ~ APA,
                     honeybee ~ APA, 
                     labels = c("pesticide" = "Pesticide",
                                "APA" = "Animal pollinated agriculture",
                                "occupancy" = "Occupancy",
                                "floral" = "Floral resources",
                                'nesting' = 'Nesting resources', 
                                'honeybee' = 'Honey bees'),
                     latent = c("floral", "nesting"),
                     exposure = "pesticide",
                     outcome = "occupancy"
)


plot(apa_dag)

##### add climate and topography for full graph 

########## check for adjustment sets when exposure is pesticide (model 1)

#build graph
apa_dag <- dagify(occupancy ~ pesticide + floral + nesting + honeybee + climate,
                  pesticide ~ APA + honeybee, 
                  nesting ~ APA, 
                  floral ~ APA + climate,
                  honeybee ~ APA, 
                  APA ~ topography + climate,
                  labels = c("pesticide" = "Pesticide",
                             "APA" = "Animal pollinated agriculture",
                             "occupancy" = "Occupancy",
                             "floral" = "Floral resources",
                             'nesting' = 'Nesting resources', 
                             'honeybee' = 'Honey bees', 
                             'topography' = "Topography and Soil properties"),
                  latent = c("floral", "nesting"),
                  exposure = "pesticide",
                  outcome = "occupancy"
)


plot(apa_dag)

## check adjustments
p2 <- ggdag::ggdag_adjustment_set(apa_dag,
                                  text = FALSE,
                                  use_labels  = "label") +
  theme_dag_blank() +
  labs(title = "Adjustment set",
       subtite = "Model for pesticide exposure and occupancy ")
p2

## adjust for APA and honey bee


###### check for adjustment sets when exposure is agriculture (model 2)

#build graph

apa_dag <- dagify(occupancy ~ pesticide + floral + nesting + honeybee + climate,
                  pesticide ~ APA + honeybee, 
                  nesting ~ APA, 
                  floral ~ APA + climate,
                  honeybee ~ APA, 
                  APA ~ topography + climate,
                  labels = c("pesticide" = "Pesticide",
                             "APA" = "Animal pollinated agriculture",
                             "occupancy" = "Occupancy",
                             "floral" = "Floral resources",
                             'nesting' = 'Nesting resources', 
                             'honeybee' = 'Honey bees', 
                             'topography' = "Topography and Soil properties"),
                  latent = c("floral", "nesting"),
                  exposure = "APA",
                  outcome = "occupancy"
)

## check for adjustments

p2 <- ggdag::ggdag_adjustment_set(apa_dag,
                                  text = FALSE,
                                  use_labels  = "label") +
  theme_dag_blank() +
  labs(title = "Adjustment set",
       subtite = "Model for pesticide exposure and occupancy ")
p2

## adjust for climate



###### check for adjustment sets when exposure is honey bees (model 3)

#build graph

apa_dag <- dagify(occupancy ~ pesticide + floral + nesting + honeybee + climate,
                  pesticide ~ APA + honeybee, 
                  nesting ~ APA, 
                  floral ~ APA + climate,
                  honeybee ~ APA, 
                  APA ~ topography + climate,
                  labels = c("pesticide" = "Pesticide",
                             "APA" = "Animal pollinated agriculture",
                             "occupancy" = "Occupancy",
                             "floral" = "Floral resources",
                             'nesting' = 'Nesting resources', 
                             'honeybee' = 'Honey bees', 
                             'topography' = "Topography and Soil properties"),
                  latent = c("floral", "nesting", "topography"),
                  exposure = "honeybee",
                  outcome = "occupancy"
)

## check for adjustments

p2 <- ggdag::ggdag_adjustment_set(apa_dag,
                                  text = FALSE,
                                  use_labels  = "label") +
  theme_dag_blank() +
  labs(title = "Adjustment set",
       subtite = "Model for pesticide exposure and occupancy ")
p2

## adjust for agriculture
