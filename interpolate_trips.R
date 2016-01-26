# Interpolate GPS data to regular time interval, either downsampling, or upsampling
# when gaps in data exist

# Load trip data ------
# Load in the filtered trip data (from trip_departure_decision_filter.R)
load("foraging_trip_info_filtered_jan2016.RData")

# Take a sample ----
# Take a sub-set of these for initial testing
# # Choose 10
# trip.sample <- sample(trips.f$trip_id,10)
# # for replication:
trip.sample <- c(2345, 3108, 2937, 1306, 2107,   37,    3, 2188,  330,  275)
# 
# # If we want to check what happens to trips with longer gaps
# trip.sample <- trips.f$trip_id[trips.f$interval_max %in% rev(sort(trips.f$interval_max))[1:5]]

# Everything
trip.sample <- trips.f$trip_id

# Get GPS data from DB ------
# Get GPS point data for these trips

# Connect to DB
library(RODBC)

# Establish a connection to the database
gps.db <- odbcConnectAccess2007('D:/Dropbox/tracking_db/GPS_db.accdb')

#See what tables are available
# sqlTables(gps.db)

# Get GPS data including meta-data
points_all <- sqlQuery(gps.db, as.is = TRUE, query=
                         paste("SELECT gps_uva_tracking_speed_3d_limited.device_info_serial, gps_uva_tracking_speed_3d_limited.date_time, gps_uva_tracking_speed_3d_limited.latitude, gps_uva_tracking_speed_3d_limited.longitude, lund_trips.trip_id
FROM lund_trips INNER JOIN gps_uva_tracking_speed_3d_limited ON lund_trips.device_info_serial = gps_uva_tracking_speed_3d_limited.device_info_serial
                               WHERE (((gps_uva_tracking_speed_3d_limited.date_time)>=[lund_trips].[start_time] And (gps_uva_tracking_speed_3d_limited.date_time)<=[lund_trips].[end_time]) AND ((lund_trips.trip_id) In (",
                               paste((trip.sample), collapse = ", "),
                              ")))
                               ORDER BY gps_uva_tracking_speed_3d_limited.device_info_serial, gps_uva_tracking_speed_3d_limited.date_time;
                               ", sep = ""))

# Fix date_time
points_all$date_time <-  as.POSIXct(strptime(points_all$date_time,
                                            format = "%Y-%m-%d %H:%M:%S",
                                            tz = "UTC"))

# Remove NA rows
points_all_nona <- na.omit(points_all)

library("adehabitatLT")

# Make these into ltraj thing from adehabitat -----
# Treat each trip as a 'burst'
points_all_nona.ltraj <- as.ltraj(points_all_nona[,4:3], points_all_nona$date_time,
                                  points_all_nona$trip_id,
                                  burst = points_all_nona$trip_id, typeII = TRUE)

# See how this looks
# summary(points_all_nona.ltraj)
# plot(points_all_nona.ltraj)

# Resample to new fixed time interval ------
# Process with redisltraj from adehabitatLT
# Try 600 s initially - or maybe even 300 s???
# points_all_nona.ltraj.600 <- redisltraj(points_all_nona.ltraj, 600, type = "time")


# Try 300 s initially
points_all_nona.ltraj.300 <- redisltraj(points_all_nona.ltraj, 300, type = "time")


plot(points_all_nona.ltraj)
plot(points_all_nona.ltraj.300)

# Convert data back to data.frame ----
points_all_nona_300s <- ld(points_all_nona.ltraj.300)


# Change radian angles to degrees
deg.relangle <- deg(points_all_nona_300s$rel.angle)


# Output data ------

out.df <- points_all_nona_300s[,c(12,3,2,1,10)]
names(out.df) <- c("trip_id", "date_time", "latitutde", "longitude", "turn_angle_rad")
out.df$turn_angle_deg <- deg.relangle


save(out.df, file = "foraging_trip_info_filtered_jan2016_points_300s.RData")

write.csv(trips.f, file = "foraging_trip_info_filtered_jan2016_points_300s.csv")
