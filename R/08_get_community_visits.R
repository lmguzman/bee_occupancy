##### Number of observations in the same day within the same 1km squared ####

library(sp)
library(rgdal)
library(geosphere)
library(lubridate)
library(tidyr)

all_obs <- fread("/Volumes/Rasters/USC/bee_occupancy/raw_data/observations/cleaned_contiguousUS_records.csv")


fil_data <- all_obs[year > 1996 & year < 2017][!is.na(year)&   !is.na(finalLatitude)& !is.na(finalLongitude)]
  
fil_data %>% 
mutate(month_year = my(paste0(month_clean, "-", year))) %>% 
  mutate(d)
  distinct() %>% # nrow() 178577
  data.table() 


### cluster points spatially -- only for observations in the same date

nrow(fil_data)


n_obs_day <- table(fil_data$month_year)

single_obs_dates <- names(n_obs_day[n_obs_day == 1])

unique_dates <- names(n_obs_day[n_obs_day > 1])


cluster_lists <- list()

sin_obs_data <- fil_data[month_year %in% ymd(single_obs_dates)]

sin_obs_data[, "cluster" := paste0(month_year, "-", 1)]

cluster_lists[[1]] <- sin_obs_data

counter <- 2

for(date_use in unique_dates){
  
  cur_date <- fil_data[month_year == date_use]
  
  lat_lon <- cur_date[,.(finalLongitude, finalLatitude)]
  
  xy <- SpatialPointsDataFrame(
    lat_lon, data.frame(ID=seq(1:nrow(lat_lon))),
    proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
  
  # use the distm function to generate a geodesic distance matrix in meters
  mdist <- distm(xy)
  
  # cluster all points using a hierarchical clustering approach
  hc <- hclust(as.dist(mdist), method="complete")
  
  # define the distance threshold
  d=1000
  
  cur_date[, "cluster" := paste0(month_year, "-", cutree(hc, h=d))]
  
  cluster_lists[[counter]] <- cur_date
  
  counter <- counter+1
}


all_clusters <- rbindlist(cluster_lists)

size_clusters <- table(all_clusters$cluster) %>% table()

# percentage of single samplings
size_clusters[1]/nrow(all_clusters)

