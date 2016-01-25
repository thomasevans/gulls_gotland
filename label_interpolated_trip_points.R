# Script to annotate the interpolated GPS data

# Load in data (from R binnary) ------
load("foraging_trip_info_filtered_jan2016_points_300s.RData")
trip.points <- out.df

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

hist(col_dist, xlim = c(0,100000), breaks = 1000)
hist(p2p_dist*1000, xlim = c(0,10000), breaks = 10000)
hist(p2p_dist*1000, xlim = c(0,1000), breaks = 100000)

source("add_alpha.R")
speed_calc <- (p2p_dist/300*1000)
plot(abs(trip.points$turn_angle_deg) ~ speed_calc, xlim = c(0,25),
     col = addalpha("black", alpha = 0.1) )

f <- 1000*p2p_dist > 150
plot(abs(trip.points$turn_angle_deg[f]) ~ speed_calc[f], xlim = c(0,25),
     col = addalpha("black", alpha = 0.1) )

hist(speed_calc[f & speed_calc >2], xlim = c(0,20), breaks = 10000)
abline(v = 2)

f2 <- f & speed_calc >2
plot(abs(trip.points$turn_angle_deg[f2]) ~ speed_calc[f2], xlim = c(0,25),
     col = addalpha("black", alpha = 0.1) )
hist(abs(trip.points$turn_angle_deg[f2]), breaks = 100)
# Label by weather on land/ sea --------
# see previous on gotland thing and the Fågelsundet land/sea script

