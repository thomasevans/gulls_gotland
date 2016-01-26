# Summarise foraging trips
# Summarise proportion of trip spent in different behaviours


# Load in trip point data ---
load("trip_points_annotated.RData")


# Package to summarise data---
library(reshape2)


# Look at speed/ step distances again ----
hist(trip.points.new$p2p_dist, xlim = c(0,200), breaks = 80000)
hist(trip.points.new$p2p_dist, xlim = c(0,1000), breaks = 80000)
hist(trip.points.new$p2p_dist, xlim = c(0,5000), breaks = 80000)
hist(trip.points.new$p2p_dist, xlim = c(0,1000), breaks = 8000)

hist(trip.points.new$p2p_dist/300, xlim = c(0,20), breaks = 1600)


# For each trip ---

# Count number of locations on Gotland
# gotland_n <- acast(trip.points.new, trip_id~gotland_on_bool, fun.aggregate = sum, value.var = gotland_on_bool)

trip.points.new$gotland_on_int <- trip.points.new$gotland_on_bool*1
trip.points.new$trip_id_int <- as.numeric(as.character(trip.points.new$trip_id))

gotland_n <- aggregate(gotland_on_int~trip_id_int,
                  data = trip.points.new,
                  FUN = sum)
names(gotland_n) <- c("trip_id_int", "gotland_n")


# Count number of locations > 3 km from the colony

# Filter out only locations > 3 km from colony
trip.points.new.sub3km <- subset(trip.points.new, col_dist > 3000)

points_over_3km <- aggregate(gotland_on_int~trip_id_int,
                       data = trip.points.new.sub3km,
                       FUN = length)
names(points_over_3km) <- c("trip_id_int", "n_points_over_3km")


points_all <- aggregate(gotland_on_int~trip_id_int,
                             data = trip.points.new,
                             FUN = length)
names(points_all) <- c("trip_id_int", "n_points")


# of these which are on Gotland, or sea?
gotland_n_3km <- aggregate(gotland_on_int~trip_id_int,
                               data = trip.points.new.sub3km,
                               FUN = sum)
names(gotland_n_3km) <- c("trip_id_int", "gotland_n_3km")


# What proportion of these are not stationary? (move >20 m in 5 minutes)
trip.points.new.sub3km.1ms <- subset(trip.points.new, p2p_dist > 20)
points_3km_over_1ms <- aggregate(gotland_on_int~trip_id_int,
                             data = trip.points.new.sub3km.1ms,
                             FUN = length)
names(points_3km_over_1ms) <- c("trip_id_int", "points_over_3km_1ms")


# of these which are on Gotland, or sea?
gotland_n_3km_1ms <- aggregate(gotland_on_int~trip_id_int,
                       data = trip.points.new.sub3km.1ms,
                       FUN = sum)
names(gotland_n_3km_1ms) <- c("trip_id_int", "gotland_n_3km_1ms")



# RESUME HERE ***** -------
# What proportion of these are no fast flight? (move <3000 m in 5 minutes)




# Assemble this on trip level
trips <- Reduce(function(...) merge(..., all=T), list(gotland_n, gotland_n_3km, gotland_n_3km_1ms,
                                                      points_all, points_3km_over_1ms, points_over_3km
                                                      ))
# Replace NA with 0
trips[is.na(trips)] <- 0


# Proportions by 'activity'
p_gotland_whole_trip <- trips$gotland_n/ trips$n_points
hist(p_gotland_whole_trip)
p_gotland_over_3km <- trips$gotland_n_3km/ trips$n_points_over_3km
hist(p_gotland_over_3km, breaks = 25)
p_gotland_over_3km_1ms <-  trips$gotland_n_3km_1ms/ trips$points_over_3km_1ms
hist(p_gotland_over_3km_1ms, breaks = 25)

summary(p_gotland_over_3km> 0.02 & p_gotland_over_3km < 0.8)
summary(p_gotland_over_3km< 0.02 )
summary(p_gotland_over_3km > 0.8)


10*300


# Label trip by period (new classification)