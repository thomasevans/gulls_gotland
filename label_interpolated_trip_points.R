# Script to annotate the interpolated GPS data

# Load in data (from R binnary) ------
load("foraging_trip_info_filtered_jan2016_points_300s.RData")
# load("foraging_trip_info_filtered_jan2016_points_300s_ext.RData")

plot(out.df$date_time)
trip.points <- out.df
trip.points$latitude <- trip.points$latitutde

n <- nrow(trip.points)

# Calculate distances between points -----
# use deg.dist function
source("deg.dist.R")

p2p_dist <- deg.dist(trip.points$longitude[-n],
                     trip.points$latitutde[-n],
                     trip.points$longitude[-1],
                     trip.points$latitutde[-1],
                     km = FALSE)
p2p_dist <- c(NA,p2p_dist)

# Set NA values for first location on each trip
trip_ids <- unique(trip.points$trip_id)

for(i in 1:length(trip_ids)){
  p2p_dist[trip.points$trip_id == trip_ids[i]][1] <- NA
  
}

# Calculate distance from colony (use some centre location) ------
col_dist <- deg.dist(trip.points$longitude,
                     trip.points$latitutde,
                     17.970992,
                     57.278884,
                     km = FALSE)

# Label by weather on land/ sea --------
# see previous on gotland thing and the Fågelsundet land/sea script


# below based on code from 'trip_details.R' from lbbg_gps R project
#' Aproximately define a polygon for Gotland, then determine
#' if trip includes points within this area.

gotland_long   <-  c(18.69902, 18.63378, 18.61279, 18.56489, 18.49318, 18.44077, 18.30666, 18.2061, 18.09348, 18.10016, 18.17383, 18.15944, 18.08482, 18.09232, 18.13166, 18.15832, 18.13748, 18.16773, 18.20654, 18.19873, 18.23659, 18.28966, 18.28202, 18.23032, 18.19989, 18.17564, 18.12695, 18.19584, 18.31607, 18.38137, 18.41146, 18.37254, 18.35202, 18.42005, 18.4743, 18.53251, 18.53104, 18.51167, 18.40778, 18.69513, 18.78849, 18.69069, 18.8719, 18.94208, 19.02026, 18.9025, 18.80552, 18.83831, 18.82224, 19.04768, 19.19782, 19.39945, 19.32853, 19.2397, 19.1699, 19.02679, 18.84733, 18.81656, 18.77957, 18.69902)

gotland_lat    <-  c(57.94159, 57.89255, 57.85448, 57.84177, 57.83054, 57.82949, 57.65937, 57.61784, 57.54344, 57.4375, 57.38398, 57.318, 57.28085, 57.26383, 57.23233, 57.18662, 57.16356, 57.13989, 57.12689, 57.05876, 57.06099, 57.09378, 57.04784, 57.03813, 57.02055, 56.98026, 56.91038, 56.89589, 56.92512, 56.97002, 56.98702, 57.01864, 57.05858, 57.09776, 57.11539, 57.11611, 57.1273, 57.13386, 57.12617, 57.22258, 57.26402, 57.29327, 57.38018, 57.40429, 57.43351, 57.45097, 57.45049, 57.60669, 57.66888, 57.73525, 57.88962, 57.94424, 57.97617, 57.99029, 57.99399, 57.93041, 57.92847, 57.87778, 57.92259, 57.94159)

library(sp)  
#Number of GPS fixes from within the Gotland polygon
on_gotland <- 
  point.in.polygon(trip.points$longitude,
                   trip.points$latitude,
                   gotland_long ,
                   gotland_lat)
summary(as.factor(on_gotland))
gotland_on_bool <- rep(NA,n)
gotland_on_bool[on_gotland == 0] <- FALSE
gotland_on_bool[on_gotland == 1] <- TRUE
summary(gotland_on_bool)


# Output the labelled data -------

# Combine to new table:
trip.points.new <- cbind.data.frame(trip.points[c(1:2,7,4:6)], col_dist, gotland_on_bool, p2p_dist)

save(trip.points.new, file = "trip_points_annotated.RData")
# save(trip.points.new, file = "trip_points_annotated_ext.RData")

