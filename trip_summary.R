# Summarise foraging trips
# Summarise proportion of trip spent in different behaviours


# Load in trip point data ---
load("trip_points_annotated.RData")


# Package to summarise data---
library(reshape2)


# Look at speed/ step distances again ----
png("distance_5_min_all.png")
hist(trip.points.new$p2p_dist, xlim = c(0,7000), breaks = 8000,
     xlab = "distance in 5 min (m)", main = "")
dev.off()

png("distance_5_min_sub200.png")
hist(trip.points.new$p2p_dist, xlim = c(0,200), breaks = 80000,
     xlab = "distance in 5 min (m)", main = "")
abline(v = 20, lwd = 2, lty = 2, col = "red")
dev.off()


png("distance_5_min_high.png")
hist(trip.points.new$p2p_dist, xlim = c(0,6000), breaks = 4000,
     xlab = "distance in 5 min (m)", main = "")
abline(v = 1500, lwd = 2, lty = 2, col = "red")
dev.off()

png("distance_5_min_retain.png")
hist(trip.points.new$p2p_dist, xlim = c(0,6000), breaks = 40000,
     xlab = "distance in 5 min (m)", main = "")
abline(v = 1500, lwd = 2, lty = 2, col = "red")
abline(v = 20, lwd = 2, lty = 2, col = "red")
dev.off()

# 
# hist(trip.points.new$p2p_dist, xlim = c(0,1000), breaks = 80000)
# hist(trip.points.new$p2p_dist, xlim = c(0,5000), breaks = 80000)
# hist(trip.points.new$p2p_dist, xlim = c(0,1000), breaks = 8000)

# hist(trip.points.new$p2p_dist/300, xlim = c(0,20), breaks = 1600)


png("col_dist_all.png")
hist(trip.points.new$col_dist/1000, xlim = c(0,150), breaks = 400,
     xlab = "Distance from colony (km)", main = "")
# abline(v = 1500, lwd = 2, lty = 2, col = "red")
# abline(v = 20, lwd = 2, lty = 2, col = "red")
dev.off()

png("col_dist_near_col.png")
hist(trip.points.new$col_dist/1000, xlim = c(0,10), breaks = 4000,
     xlab = "Distance from colony (km)", main = "")
abline(v = 3, lwd = 2, lty = 2, col = "red")
# abline(v = 20, lwd = 2, lty = 2, col = "red")
dev.off()



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
# What proportion of these are fast flight? (move <1500 m in 5 minutes)
trip.points.new.sub3km.sub3move <- subset(trip.points.new.sub3km, p2p_dist < 1500)

hist(trip.points.new.sub3km.sub3move$p2p_dist, breaks = 40)
plot(trip.points.new.sub3km.sub3move$p2p_dist, abs(trip.points.new.sub3km.sub3move$turn_angle_deg))


points_3km_over_1ms_sub3move <- aggregate(gotland_on_int~trip_id_int,
                                 data = trip.points.new.sub3km.sub3move,
                                 FUN = length)
names(points_3km_over_1ms_sub3move) <- c("trip_id_int", "points_3km_over_1ms_sub3move")

# of these which are on Gotland, or sea?
gotland_n_3km_1ms_sub3move <- aggregate(gotland_on_int~trip_id_int,
                               data = trip.points.new.sub3km.sub3move,
                               FUN = sum)
names(gotland_n_3km_1ms_sub3move) <- c("trip_id_int", "gotland_n_3km_1ms_sub3move")




# Assemble this on trip level
trips <- Reduce(function(...) merge(..., all=T), list(gotland_n, gotland_n_3km, gotland_n_3km_1ms,
                                                      points_all, points_3km_over_1ms, points_over_3km,
                                                      points_3km_over_1ms_sub3move, gotland_n_3km_1ms_sub3move
                                                      ))
# Replace NA with 0
trips[is.na(trips)] <- 0


# Proportions by 'activity'
p_gotland_whole_trip <- trips$gotland_n/ trips$n_points
png("Proportion_all_points_gotland.png")
hist(p_gotland_whole_trip*100,
     xlab = "% of trip on Gotland", main = "")
dev.off()

p_gotland_over_3km <- trips$gotland_n_3km/ trips$n_points_over_3km
hist(p_gotland_over_3km, breaks = 25)
p_gotland_over_3km_1ms <-  trips$gotland_n_3km_1ms/ trips$points_over_3km_1ms
hist(p_gotland_over_3km_1ms, breaks = 25)
p_gotland_over_3km_1ms_sub3move <-  trips$gotland_n_3km_1ms_sub3move/ trips$points_3km_over_1ms_sub3move

png("Proportion_filter_points_gotland.png")
hist(p_gotland_over_3km_1ms_sub3move*100,
     xlab = "% of 'foraging time' spent on Gotland", main = "")
dev.off()
thing <- ecdf(p_gotland_over_3km_1ms_sub3move*100)
# thing
summary(thing)
# quantile(thing, 0.75)
png("Proportion_filter_points_gotland_cumulative.png")
plot(ecdf(p_gotland_over_3km_1ms_sub3move*100), main = "",
     xlab = "% of 'foraging time' spent on Gotland", ylab = "Cumulative proportion of trips")
abline(v = c(5,95), lwd = 2, lty = 2, col = "red")
dev.off()

png("Proportion_filter_points_gotland_cumulative_10_90.png")
plot(ecdf(p_gotland_over_3km_1ms_sub3move*100), main = "",
     xlab = "% of 'foraging time' spent on Gotland", ylab = "Cumulative proportion of trips")
abline(v = c(10,90), lwd = 2, lty = 2, col = "red")
dev.off()

summary(p_gotland_over_3km_1ms_sub3move> 0.05 & p_gotland_over_3km_1ms_sub3move < 0.95)
summary(p_gotland_over_3km_1ms_sub3move< 0.05)
summary(p_gotland_over_3km_1ms_sub3move > 0.95)

summary(p_gotland_over_3km_1ms_sub3move> 0.10 & p_gotland_over_3km_1ms_sub3move < 0.90)
summary(p_gotland_over_3km_1ms_sub3move< 0.10)
summary(p_gotland_over_3km_1ms_sub3move > 0.90)


summary(p_gotland_over_3km> 0.02 & p_gotland_over_3km < 0.8)
summary(p_gotland_over_3km< 0.02 )
summary(p_gotland_over_3km > 0.8)


summary(p_gotland_over_3km> 0.6 & p_gotland_over_3km < 0.7)

summary(p_gotland_over_3km> 0.8 & p_gotland_over_3km < 0.85)

summary(p_gotland_over_3km> 0.9 & p_gotland_over_3km < 0.95)

summary(p_gotland_over_3km> 0.95 & p_gotland_over_3km < 1.)

# 
# 
# 10*300


# Label trip by period (new classification)

# ######### If crash - start again here! ######### -----------------
load("trip_summary_data.RData")

# Plot examples of trips -----
source("plot_trips_fun.R")

f <- (p_gotland_over_3km_1ms_sub3move >= 0.00 & p_gotland_over_3km_1ms_sub3move <= 0.05)

f2 <- trip.points.new$col_dist > 3000 & trip.points.new$p2p_dist < 1500 &
  trip.points.new$p2p_dist > 20  & trip.points.new$gotland_on_bool

f3 <- trip.points.new$col_dist > 3000 & trip.points.new$p2p_dist < 1500 &
  trip.points.new$p2p_dist > 20  & !trip.points.new$gotland_on_bool


trip_ids <- unique(trips$trip_id_int[f])

# If too many trips to plot, plot a sample
if(length(trip_ids > 8)){
  trip_ids <- sample(trip_ids,8)
}

points.f <- trip.points.new$trip_id %in% trip_ids
# summary(points.f)
for_points_got <- (f2 & points.f)[points.f]
for_points_sea <- (f3 & points.f)[points.f]

# summary(for_points)
dpi = 600
png("example_trips_0.0_0.05_y_2.png", width = 5*dpi, height = 5*dpi, res = dpi)
plot.trips(long = trip.points.new$longitude[points.f],
           lat = trip.points.new$latitude[points.f],
           trip_ids = trip.points.new$trip_id[points.f],
           for_points_got =  for_points_got,
           for_points_sea =  for_points_sea)
dev.off()

# length(trip.points.new$longitude[points.f])
# length(for_points)


# Make some fancy plot of these trips ------


# Proportion for cut-off (for sorting)
trip_id <- trips$trip_id_int
p_for_got <- trips$gotland_n_3km_1ms_sub3move/trips$points_3km_over_1ms_sub3move
p_got <- trips$gotland_n_3km_1ms_sub3move/trips$n_points
p_sea <- (trips$points_3km_over_1ms_sub3move-trips$gotland_n_3km_1ms_sub3move)/trips$n_points
p_oth <- (trips$n_points-trips$points_3km_over_1ms_sub3move) / trips$n_points
hist(p_oth)
hist(p_sea)
hist(p_got)

rank_trip <- order(p_for_got, -(p_sea), p_got)
rank_id <- 1:length(rank_trip)

gg_df <- cbind.data.frame(p_got,p_sea,p_oth)
gg_df[is.na(gg_df)] <- 0
gg_df <- gg_df[rank_trip,]
gg_df <- cbind.data.frame(rank_id,gg_df)

gg_df_new <- melt(gg_df, id=c("rank_id"))
gg_df_new$variable <-  factor(gg_df_new$variable, levels = c("p_sea","p_oth","p_got"))
levels(gg_df_new$variable) <- c("Sea", "Other", "Land")

# ?melt

library(ggplot2)
library(scales)


# sb <- ggplot(gg_df_new, aes(x=trip_id,value,fill=variable)) +
#   geom_bar(binwidth = 1)
# sb
# 
# p<-ggplot(gg_df_new,stat="identity")
# p<-p+geom_bar(aes(x=rank.trip,value,fill=variable))
# p
# 
# q <- ggplot(gg_df_new, aes(x = trip_id, value, fill=variable))
# q + geom_bar(aes(fill = variable))
# 
# 
png("ggplot_prop_land_sea_sep.png", , width = 10*dpi, height = 5*dpi, res = dpi)
ggplot(gg_df_new, aes(x = rank_id, y = value, fill = variable, order=variable)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values=c("light blue"," dark grey","dark green")) +
  geom_vline(xintercept = c(711,803), colour="red", linetype = "longdash", lwd =1.5)
dev.off()



# Assemble classification table -------
trip_id <- trips$trip_id_int
p_for_got <- trips$gotland_n_3km_1ms_sub3move/trips$points_3km_over_1ms_sub3move
p_got <- trips$gotland_n_3km_1ms_sub3move/trips$n_points
p_sea <- (trips$points_3km_over_1ms_sub3move-trips$gotland_n_3km_1ms_sub3move)/trips$n_points
p_oth <- (trips$n_points-trips$points_3km_over_1ms_sub3move) / trips$n_points
p_got_whole_trip <- trips$gotland_n/trips$n_points

df_combined <- cbind.data.frame(trip_id,p_for_got,p_got,p_sea,p_oth,p_got_whole_trip)
class_2 <- rep("LAND",nrow(df_combined))
class_2[p_for_got<0.5] <- "SEA"
class_2[p_oth == 1] <- NA
class_3  <- rep("MIX",nrow(df_combined))
class_3[p_for_got<0.1] <- "SEA"
class_3[p_for_got>0.9] <- "LAND"
class_3[p_oth == 1] <- NA
class_original  <- rep("NA",nrow(df_combined))
class_original[p_got_whole_trip<0.2] <- "SEA"
class_original[p_got_whole_trip>0.2] <- "LAND"



df_combined <- cbind.data.frame(df_combined,class_2,class_3,class_original)

mytable <- table(class_2, class_original) 
ftable(mytable)

mytable <- table(class_3, class_original) 
ftable(mytable)


# Summarise by date periods for plotting -----
trips_x <- trips
names(trips_x)[1] <- "trip_id"
load("foraging_trip_info.RData")
trips_detailed <- merge(df_combined, trips_x, "trip_id")
trips_detailed <- merge(trips_detailed, trips, "trip_id")

trips_detailed$date_time <- as.POSIXct(paste(trips_detailed$date_utc,
                                             trips_detailed$time_utc, ssep = " "),
                                       tz = "UTC")





df.period <- data.frame()
df.period$year <- NULL
df.period$season <- NULL
df.period$date_mid <- NULL
df.period$p_got <- NULL
df.period$p_mix <- NULL
df.period$p_sea <- NULL
df.period$n_got <- NULL
df.period$n_mix <- NULL
df.period$n_sea <- NULL
df.period$n_tot <- NULL




years <- 2011:2013
# For each year (3)
i <- 1
  start_date <- as.POSIXct(paste(years[i], "-05-20 00:00", sep = ""), tz = "UTC")
  
  # For each date period in year (4)
    
    # add 6 days to get the end date
    end_date <- start_date + 6*24*60*60
    
    # Trip filter
    tf <- (trips_detailed$date_time > start_date) & (trips_detailed$date_time < end_date)
    # summary(tf)

    



