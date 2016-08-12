# Summarise foraging trips
# Summarise proportion of trip spent in different behaviours


# Load in trip point data ---
# load("trip_points_annotated.RData")
load("trip_points_annotated_ext.RData")

# plot(trip.points.new$date_time)

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


win.metafile("gps_move_distance.wmf", width = 14, height = 7)
par(mfrow=c(1,2))
hist(trip.points.new$p2p_dist[trip.points.new$p2p_dist <10000]/1000, breaks = 50,
     xlab = "Distance in 5 min (km)",
     ylab = "N interpolated GPS locations",
     main = "All GPS locations")
abline(v = 1.5, lwd = 2, lty = 2, col = "red")
hist(trip.points.new$p2p_dist[trip.points.new$p2p_dist <200], breaks = 50,
     xlab = "Distance in 5 min (m)",
     ylab = "N interpolated GPS locations",
     main = "<200 m")
abline(v = 20, lwd = 2, lty = 2, col = "red")
dev.off()

1500/300
20/300
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


win.metafile("gps_col_dist.wmf", width = 14, height = 7)
par(mfrow=c(1,2))
hist(trip.points.new$col_dist/1000, xlim = c(0,150), breaks = 100,
     xlab = "Distance from colony (km)",
     ylab = "N interpolated GPS locations",
     main = "All locations")

hist(trip.points.new$col_dist[trip.points.new$col_dist < 10000]/1000,
     xlim = c(0,10), breaks = 50,
     xlab = "Distance from colony (km)",
     ylab = "N interpolated GPS locations",
     main = "<10 km")
abline(v = 3, lwd = 2, lty = 2, col = "red")
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



f <- (p_gotland_over_3km_1ms_sub3move >= 0.2 & p_gotland_over_3km_1ms_sub3move <= 0.8)

f2 <- trip.points.new$col_dist > 3000 & trip.points.new$p2p_dist < 1500 &
  trip.points.new$p2p_dist > 20  & trip.points.new$gotland_on_bool

f3 <- trip.points.new$col_dist > 3000 & trip.points.new$p2p_dist < 1500 &
  trip.points.new$p2p_dist > 20  & !trip.points.new$gotland_on_bool

# marine trips
# 2728 2559 2342 2000 3308 1639 3113  347 3116 1305

# Land trips
# 54 2449   79 2245 1393 3259 1368  710  500   24
# 1592 1441  272 1726  957  476 1591  986 2353  260
# 2719 1357 1968 1573 2091 1361 1439 1394 1564 2266
trip_ids <- unique(trips$trip_id_int[f])

# Mixed (range 20 - 80 %)
# 2757 2241 2275 1320   12 3264 1760 1754 1365 2953

# If too many trips to plot, plot a sample
# if(length(trip_ids > 8)){
  trip_ids <- sample(trip_ids,10)
# }

points.f <- trip.points.new$trip_id %in% trip_ids
# summary(points.f)
for_points_got <- (f2 & points.f)[points.f]
for_points_sea <- (f3 & points.f)[points.f]
points_other <- (!f2 & !f3 & points.f)[points.f]

# summary(for_points)
dpi = 1000
png("example_trips_land_0.2_0.8_mixed_1.png", width = 5*dpi, height = 8*dpi, res = dpi)
# cairo_ps(filename = "example_trips_land_0.95more_4.ps", width = 5, height = 8)
# pdf("example_trips_sea_0.05less_4.pdf", width = 5, height = 8)
# postscript("example_trips_sea_0.05less_4.eps", width = 5, height = 8, horizontal = FALSE, onefile = FALSE)
# library(devEMF)
# emf(file="example_trips_new.emf", bg="white", width=5, height=8, family="Calibri", pointsize=20)
# win.metafile(filename = "example_trips_sea_0.05less_4.wmf", width = 5, height = 8, pointsize = 12,
#              restoreConsole = TRUE)
plot.trips(long = trip.points.new$longitude[points.f],
           lat = trip.points.new$latitude[points.f],
           trip_ids = trip.points.new$trip_id[points.f],
           for_points_got =  for_points_got,
           for_points_sea =  for_points_sea,
           other_points = points_other)
dev.off()
# warnings()

# length(trip.points.new$longitude[points.f])
# length(for_points)


# Plot long trips ----
# trip.points.new$trip_id

trips.detail <- read.csv("trips_details_2016_01_29.csv", header = TRUE)

f <- trips.detail$duration_s > 1*24*60*60
hist(trips.detail$p_for_got[f], breaks = 20)
summary(f)
f2 <- trip.points.new$col_dist > 3000 & trip.points.new$p2p_dist < 1500 &
  trip.points.new$p2p_dist > 20  & trip.points.new$gotland_on_bool

f3 <- trip.points.new$col_dist > 3000 & trip.points.new$p2p_dist < 1500 &
  trip.points.new$p2p_dist > 20  & !trip.points.new$gotland_on_bool


# trip_ids <- unique(trips$trip_id_int[f])
trip_ids <- trips.detail$trip_id[f]


# If too many trips to plot, plot a sample
if(length(trip_ids) > 8){
  trip_ids <- sample(trip_ids,8)
}

str(trip_ids)
str(trip.points.new$trip_id)

points.f <- trip.points.new$trip_id %in% trip_ids
# summary(points.f)
for_points_got <- (f2 & points.f)[points.f]
for_points_sea <- (f3 & points.f)[points.f]

png("long_24h_trips_sample.png")
plot.trips(long = trip.points.new$longitude[points.f],
           lat = trip.points.new$latitude[points.f],
           trip_ids = trip.points.new$trip_id[points.f],
           for_points_got =  for_points_got,
           for_points_sea =  for_points_sea)

dev.off()

# Make some fancy plot of these trips ------
library(reshape2)

# Proportion for cut-off (for sorting)
trip_id <- trips$trip_id_int
p_for_got <- trips$gotland_n_3km_1ms_sub3move/trips$points_3km_over_1ms_sub3move
p_got <- trips$gotland_n_3km_1ms_sub3move/trips$n_points
p_sea <- (trips$points_3km_over_1ms_sub3move-trips$gotland_n_3km_1ms_sub3move)/trips$n_points
p_oth <- (trips$n_points-trips$points_3km_over_1ms_sub3move) / trips$n_points
hist(p_oth)
hist(p_sea)
hist(p_got)

rank_trip <- order(-p_for_got, -(p_got), (p_sea))
rank_id <- 1:length(rank_trip)

gg_df <- cbind.data.frame(p_got,p_sea,p_oth)
gg_df[is.na(gg_df)] <- 0
gg_df <- gg_df[rank_trip,]
gg_df <- cbind.data.frame(rank_id,gg_df)

gg_df_new <- melt(gg_df, id=c("rank_id"))
gg_df_new$variable <-  factor(gg_df_new$variable, levels = c("p_got","p_oth","p_sea"))
levels(gg_df_new$variable) <- c("Land","Other","Sea")

# ?melt

library(ggplot2)
library(scales)

gg_df_new$value <- gg_df_new$value*100
dpi <- 1000
png("ggplot_prop_land_sea_sep_fig_NEW.png", , width = 10*dpi, height = 5*dpi, res = dpi)

win.metafile("ggplot_prop_land_sea_sep_fig_NEW.wmf",width=10, height=5)
ggplot(gg_df_new, aes(x = rank_id, y = value, fill = variable, order=variable)) +
  geom_bar(stat = "identity", width = 1) +
  # facet_grid(Year ~ ., scale = "fixed") +
  scale_fill_manual(values=c("dark green"," dark grey","dark blue")) +
  # geom_vline(xintercept = c(711,803), colour="red", linetype = "longdash", lwd =1.5) +
  scale_y_continuous(name="Proportion of foraging trip (%)", breaks=seq(0,100,20)) + 
  scale_x_continuous(name="Trip (rank order)") +
  # scale_y_continuous() +
  theme(axis.title = element_text(face="bold", size=18),
        axis.text  = element_text(size = 12),
        legend.title=element_blank(),
        legend.text = element_text(size = 14))
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



# for 3 years ---- 


df.period <- data.frame(year= integer(36),season= character(36),
                        date_mid= .POSIXct(character(36), tz = "UTC"),p_got= numeric(36),
                        p_mix= numeric(36),p_sea= numeric(36),
                        n_got= integer(36),n_mix= integer(36),
                        n_sea= integer(36),n_tot= integer(36),
                        p_for_got_mean= numeric(36),
                        p_for_got_med = numeric(36))

str(trips)

seasons <- as.factor(c("incubation", "chick_early", "chick_late"))

levels(df.period$season) <- levels(seasons)


years <- 2011:2013
# For each year (3)
# i <- 2
ix <- 1
  for(i in 1:3){
    start_date <- as.POSIXct(paste(years[i], "-05-20 00:00:00", sep = ""), tz = "UTC")
    
    # For each date period in year (4)
    # iz  
    # iz <- 1
    for(iz in 1:12){
      
      # add 6 days to get the end date
      end_date <- start_date + 6*24*60*60
      
      # Trip filter
      tf <- (trips_detailed$date_time > start_date) & (trips_detailed$date_time < end_date)
      # summary(tf)
      
      df.period$year[ix] <- years[i]
      if(iz <= 4) {df.period$season[ix] <- seasons[1]}else if(iz <= 8){
        df.period$season[ix] <- seasons[2]
      } else df.period$season[ix] <- seasons[3]
      # df.period$season[ix] <- switch(floor(iz/4)+1, seasons[1], seasons[2], seasons[3])
      df.period$date_mid[ix] <- start_date + 3*24*60*60
      df.period$n_got[ix] <- sum(trips_detailed$class_3[tf] == "LAND", na.rm = TRUE)
      df.period$n_mix[ix] <- sum(trips_detailed$class_3[tf] == "MIX", na.rm = TRUE)
      df.period$n_sea[ix] <- sum(trips_detailed$class_3[tf] == "SEA", na.rm = TRUE)
      df.period$n_tot[ix] <- length(trips_detailed$class_3[tf])
      df.period$p_got[ix] <- df.period$n_got[ix]/df.period$n_tot[ix]
      df.period$p_mix[ix] <- df.period$n_mix[ix]/df.period$n_tot[ix]
      df.period$p_sea[ix] <- df.period$n_sea[ix]/df.period$n_tot[ix]
      df.period$p_for_got_mean[ix] <- mean(trips_detailed$p_for_got[tf], na.rm = TRUE)
      df.period$p_for_got_med[ix] <- median(trips_detailed$p_for_got[tf], na.rm = TRUE)
      ix <- ix +1
      start_date <- start_date + 5*24*60*60
    }
    
  }
# warnings()


# Make nice ggplot thing of trip types by date -----


# # Example code from: http://stackoverflow.com/questions/11458349/add-text-to-a-faceted-plot-in-ggplot2-with-dates-on-x-axis
# ggplot(dat, aes(x=date, y=value, color=location, group=location)) + 
#   geom_line()+
#   facet_grid(product ~ ., scale = "free_y")
# 
# 
# # Previous code
# png("ggplot_prop_land_sea_sep.png", , width = 10*dpi, height = 5*dpi, res = dpi)
# ggplot(gg_df_new, aes(x = rank_id, y = value, fill = variable, order=variable)) +
#   geom_bar(stat = "identity") +
#   scale_fill_manual(values=c("light blue"," dark grey","dark green")) +
#   geom_vline(xintercept = c(711,803), colour="red", linetype = "longdash", lwd =1.5)
# dev.off()
# 
# gg_df_new <- melt(gg_df, id=c("rank_id"))
# gg_df_new$variable <-  factor(gg_df_new$variable, levels = c("p_sea","p_oth","p_got"))
# levels(gg_df_new$variable) <- c("Sea", "Other", "Land")


# New code
library(ggplot2)
library(scales)
library(reshape2)

gg_trips_period_df <- cbind.data.frame(df.period$year, df.period$date_mid,
                                       df.period$p_got*100, df.period$p_mix*100,
                                       df.period$p_sea*100)
names(gg_trips_period_df) <- c("Year", "Date", "Land", "Mixed", "Sea")
str(gg_trips_period_df)

gg_trips_period_df$Date <- as.Date(paste("2011-", format(gg_trips_period_df$Date, "%m-%d"), sep = ""))

gg_df_period_new <- melt(gg_trips_period_df, id.vars = c("Year", "Date"))

date.per <- as.numeric(c(as.Date("2011-06-10"), as.Date("2011-07-01")))
# str(gg_df_period_new)
# str(gg_df_period_new$Year)
# dpi = 600
# ann_text <- data.frame(Date = ,
#                        value = 90,lab = c("Incubation", "Chick-rearing 1",
#                                           "Chick-rearing 2"),
#                        Year = 2011)
# ann_text$Year <- as.integer(ann_text$Year)
# 
# # vars <- data.frame(expand.grid(levels(gg_df_period_new$Year)))
# 
# dat <- data.frame(x = as.Date(c("2011-05-30", "2011-06-15", "2011-07-05")),
#                   y = rep(90, 3), as.integer(2011), labs=c("Incubation", "Chick-rearing 1",
#                                                "Chick-rearing 2"))
# 
# p + geom_text(aes(x, y, label=labs, group=NULL),data=dat)

dpi <- 1000
library(scales) # for date_breaks()
# png("ggplot_prop_land_sea_sep_fig_2.png", , width = 10*dpi, height = 5*dpi, res = dpi)
png("ggplot_prop_land_sea_mix_date_new.png", width = 10*dpi, height = 6*dpi, res = dpi)
ggplot(gg_df_period_new, aes(x = Date, y = value, color = variable)) + 
  # scale_x_date(date_minor_breaks = "5 day") +
  geom_line(lwd = 1)+
  scale_color_manual(values=c("dark green", "magenta", "dark blue")) +
  facet_grid(Year ~ ., scale = "fixed") +
  # ylim(0, 100) +
  geom_vline(xintercept = date.per,
             colour="dark grey", linetype = "longdash", lwd =1.5) +
  scale_y_continuous(name="Proporition of trips (%)", breaks=seq(0,100,20),
                     limits = c(0,100)) + 
  # scale_x_continuous(name="Date") +
  # scale_y_continuous() +
  scale_x_date(breaks = date_breaks("10 day"),
                     minor_breaks = date_breaks("5 day"),
               labels=date_format("%d-%b"))+
               # date_labels = "%d-%b") +
  # scale_x_date(date_labels = "%d-%b")+
  # ylim(0,100)+
  theme(axis.title = element_text(face="bold", size=18),
        axis.text  = element_text(size = 12),
        legend.title = element_blank(),
        legend.text = element_text(size = 14))
  # + geom_text(aes(x, y, label=labs, group=NULL),data=dat)
  # annotate("text", x = (c(as.Date("2011-05-30"))), y = 90, label = "Some text")

dev.off()
# getwd()


# All years pooled ----

df.period <- NULL
df.period <- data.frame(season= character(12),
                        date_mid= .POSIXct(character(12), tz = "UTC"),p_got= numeric(12),
                        p_mix= numeric(12),p_sea= numeric(12),
                        n_got= integer(12),n_mix= integer(12),
                        n_sea= integer(12),n_tot= integer(12),
                        p_for_got_mean= numeric(12),
                        p_for_got_med = numeric(12))

str(trips)

seasons <- as.factor(c("incubation", "chick_early", "chick_late"))

levels(df.period$season) <- levels(seasons)


years <- 2011:2013# For each year (3)
# i <- 2
# ix <- 1
# for(i in 1:3){
  start_date <- as.POSIXct(paste(years, "-05-20 00:00:00", sep = ""), tz = "UTC")
  
  # For each date period in year (4)
  # iz  
  # iz <- 1
  for(iz in 1:12){
    
    # add 6 days to get the end date
    end_date <- start_date + 6*24*60*60
    
    # Trip filter
    tf <- ((trips_detailed$date_time > start_date[1]) & (trips_detailed$date_time < end_date[1])) |
      (trips_detailed$date_time > start_date[2]) & (trips_detailed$date_time < end_date[2]) |
      (trips_detailed$date_time > start_date[3]) & (trips_detailed$date_time < end_date[3])
    # summary(tf)
    
    # df.period$year[ix] <- years[i]
    if(iz <= 4) {df.period$season[iz] <- seasons[1]}else if(iz <= 8){
      df.period$season[iz] <- seasons[2]
    } else df.period$season[iz] <- seasons[3]
    # df.period$season[ix] <- switch(floor(iz/4)+1, seasons[1], seasons[2], seasons[3])
    df.period$date_mid[iz] <- start_date + 3*24*60*60
    df.period$n_got[iz] <- sum(trips_detailed$class_3[tf] == "LAND", na.rm = TRUE)
    df.period$n_mix[iz] <- sum(trips_detailed$class_3[tf] == "MIX", na.rm = TRUE)
    df.period$n_sea[iz] <- sum(trips_detailed$class_3[tf] == "SEA", na.rm = TRUE)
    df.period$n_tot[iz] <- length(trips_detailed$class_3[tf])
    df.period$p_got[iz] <- df.period$n_got[iz]/df.period$n_tot[iz]
    df.period$p_mix[iz] <- df.period$n_mix[iz]/df.period$n_tot[iz]
    df.period$p_sea[iz] <- df.period$n_sea[iz]/df.period$n_tot[iz]
    df.period$p_for_got_mean[iz] <- mean(trips_detailed$p_for_got[tf], na.rm = TRUE)
    df.period$p_for_got_med[iz] <- median(trips_detailed$p_for_got[tf], na.rm = TRUE)
    # ix <- ix +1
    start_date <- start_date + 5*24*60*60
  }
  

library(ggplot2)
library(scales)
library(reshape2)

gg_trips_period_df <- cbind.data.frame(df.period$date_mid,
                                       df.period$p_got*100, df.period$p_mix*100,
                                       df.period$p_sea*100)
names(gg_trips_period_df) <- c("Date", "Land", "Mixed", "Sea")
str(gg_trips_period_df)

gg_trips_period_df$Date <- as.Date(paste("2011-", format(gg_trips_period_df$Date, "%m-%d"), sep = ""))

gg_df_period_new <- melt(gg_trips_period_df, id.vars = c("Date"))

date.per <- as.numeric(c(as.Date("2011-06-10"), as.Date("2011-07-01")))

dpi <- 1000
library(scales) # for date_breaks()
# png("ggplot_prop_land_sea_sep_fig_2.png", , width = 10*dpi, height = 5*dpi, res = dpi)
png("ggplot_prop_land_sea_mix_date_new_allyearspooled_alt_theme.png", width = 10*dpi, height = 4*dpi, res = dpi)
ggplot(gg_df_period_new, aes(x = Date, y = value, color = variable)) + 
  # scale_x_date(date_minor_breaks = "5 day") +
  geom_line(lwd = 1)+
  scale_color_manual(values=c("dark green", "magenta", "dark blue")) +
  # facet_grid(Year ~ ., scale = "fixed") +
  # ylim(0, 100) +
  geom_vline(xintercept = date.per,
             colour="dark grey", linetype = "longdash", lwd =1.5) +
  scale_y_continuous(name="Proporition of trips (%)", breaks=seq(0,100,20),
                     limits = c(0,100)) + 
  # scale_x_continuous(name="Date") +
  # scale_y_continuous() +
  scale_x_date(breaks = date_breaks("10 day"),
               minor_breaks = date_breaks("5 day"),
               labels=date_format("%d-%b"))+
  # date_labels = "%d-%b") +
  # scale_x_date(date_labels = "%d-%b")+
  # ylim(0,100)+
  theme_bw() +
  theme(axis.title = element_text(face="bold", size=18),
        axis.text  = element_text(size = 12),
        legend.title = element_blank(),
        legend.text = element_text(size = 14))
# + geom_text(aes(x, y, label=labs, group=NULL),data=dat)
# annotate("text", x = (c(as.Date("2011-05-30"))), y = 90, label = "Some text")

dev.off()

# Prepare trip info for export ------
years <- 2011:2013
pre_laying.starts <- as.POSIXct(paste(years, "-04-30 00:00:00", sep = ""), tz = "UTC")
incubation.starts <- as.POSIXct(paste(years, "-05-20 00:00:00", sep = ""), tz = "UTC")
chick_rearing.starts <- as.POSIXct(paste(years, "-06-10 00:00:00", sep = ""), tz = "UTC") 
chick_rearing_late.starts <- as.POSIXct(paste(years, "-07-01 00:00:00", sep = ""), tz = "UTC")
chick_rearing_late.ends <- as.POSIXct(paste(years, "-07-22 00:00:00", sep = ""), tz = "UTC")    
    

period <- rep(NA, nrow(trips_detailed))
for(i in 1:3){
  period[(trips_detailed$date_time > pre_laying.starts[i]) & (trips_detailed$date_time < incubation.starts[i])] <- "Pre_laying"
  period[(trips_detailed$date_time > incubation.starts[i]) & (trips_detailed$date_time < chick_rearing.starts[i])] <- "Incubation"
  period[(trips_detailed$date_time > chick_rearing.starts[i]) & (trips_detailed$date_time < chick_rearing_late.starts[i])] <- "Chick_1"
  period[(trips_detailed$date_time > chick_rearing_late.starts[i]) & (trips_detailed$date_time < chick_rearing_late.ends[i])] <- "Chick_2"

}

summary(as.factor(period))

years.trip <- format(trips_detailed$date_time, "%Y")


trips_detailed_new <- cbind(trips_detailed, period, years.trip)

# ?write.csv
write.csv(trips_detailed_new, file = "trips_details_2016_01_29.csv")

# write.csv(trips_detailed_new, file = "trips_details_2016_01_29_extended.csv")


# Time of day figure -----
# Based on script in 'time_of_day_figure.R' script

library("lubridate")

win.metafile(filename = "time_of_day_new_percent.wmf", width = 10, height = 6, pointsize = 12)


time_from_sunrise_h <- trips_detailed$sunrise_dif_s/60/60
range(time_from_sunrise_h)

range.fun <- function(x){
  if(x > 20){x <- (x -24)}
  return(x)
}
time_from_sunrise_h <- sapply(time_from_sunrise_h, range.fun)
range(time_from_sunrise_h)
# hist(time_from_sunrise_h)

# Plot all trips first
hist(time_from_sunrise_h, xlim = c(-4,20), xaxs = "i", yaxs = "i",
     ylim = c(0,110), breaks = 24, col='white',
     border = TRUE, main = "", xlab = "Time since sunrise (h)",
     ylab = "Trip departures per hour (%)",
     xaxt = "n",
     yaxt = "n",
     las = 1,
     cex.lab = 1.4,
     cex.axt = 1.2,
     font.lab = 2,
     mgp=c(3,1,0))

length(time_from_sunrise_h)
100/1038

steps.y <- seq(0,10,2)
pos.y <- steps.y*1038*0.01

day_length <- trips_detailed$sunset_date_time - trips_detailed$sunrise_date_time
min(day_length)
max(day_length)
# hist(as.numeric(day_length))
  
rect(-4, 0, 24, 110, density = NULL, angle = 45,
     col = "light yellow", border = NA)
rect(min(day_length), 0, 24, 110, density = NULL, angle = 45,
     col = "light grey", border = NA)
rect(max(day_length), 0, 24, 110, density = NULL, angle = 45,
     col = "dark grey", border = NA)
rect(-4, 0, 0, 110, density = NULL, angle = 45,
     col = "dark grey", border = NA)
# ?rect


# Marine trips (actually all - but will be covered)
hist(time_from_sunrise_h,
    breaks = 24, col='dark blue',
    border = "white", add = TRUE)



# Of which mixed trips
hist(time_from_sunrise_h[p_for_got>0.05],
     breaks = 24, col='magenta',
     border = "white", add = TRUE)


# Of which land trips
hist(time_from_sunrise_h[p_for_got>0.95],
     breaks = 24, col='dark green',
     border = "white", add = TRUE)

axis(1, at = seq(-4,20,2), labels = seq(-4,20,2), pos = 0,
     cex = 1.2)
# axis(2, pos = -4,
#      cex = 1.2, las = 1)
#perc
axis(2, pos = -4, at = pos.y, labels = steps.y,
     cex = 1.2, las = 1)
# Make it a bit prettier
# box()
dev.off()


# Sample sizes ------
summary(trips_detailed$class_3)
length(trips_detailed$class_3)
summary(trips_detailed$class_3)/1038

# Place holding - figure of coefs from models ------

# First make sure to use the standardized model to allow for comparison
stdz.mod.7<-standardize(mod.7, standardize.y=FALSE)
# Check this looks sensible
summary(stdz.mod.7)

# Get confidence intervals for coeficients
stdz.mod.7.ci.Wald <- confint(stdz.mod.7, method="Wald")

# Need this package for plots:
library(lattice)

# I worked this out based on code here:
# http://www.ashander.info/posts/2015/04/D-RUG-mixed-effects-viz/

# Make a data.frame of the CI
ci_dat <-stdz.mod.7.ci.Wald
ci_dat <- cbind(ci_dat, mean=rowMeans(ci_dat))
ci_df <- data.frame(coef = row.names(ci_dat), ci_dat)
names(ci_df)[2:3] <- c('lwr', 'upr')

# View current coeficient names:
ci_df$coef

# Make a new vector of coeficient names (need to change these to what is
# sensible based on the model)
ci_df$coef_new <- c(NA, "(intercept)", "Stage - Chick 1", "Stage - Chick 2",
                    "Cloud", "Temperature", "Precipitation",
                    "Sunrise proximity")

# If you want the coeficients displayed in a different order to the current
# Here we sort them in order of the coeficient value
ci_df_sort <- ci_df[order(ci_df$mean),]
ci_df_sort$coef_new <- factor(ci_df_sort$coef_new, levels = unique(ci_df_sort$coef_new))


# Plot the figure
lattice::dotplot(coef_new ~ mean, ci_df_sort, xlim = c(-5,5),
                 #                  cexl.lab = 1.5, cex.axis = 1.5,
                 xlab = list("Effect (log-odds of terrestrial foraging)",cex=1.3),
                 panel = function(x, y) {
                   panel.segments(ci_df_sort$lwr, y, ci_df_sort$upr, y, lwd =2)
                   panel.xyplot(x, y, pch=18, cex = 1.2, col = "black")
                   panel.abline(v=0, lty=2)
                 },scales=list(y=list(cex=1.2), x = list(cex = 1.2))
)








# Make vector of day numbers  ------

get.day.fun <- function(x, y){
  if(y == "2011"){day1 =  as.POSIXct("2011-05-20")} else {
    if(y == "2012"){
      day1 =  as.POSIXct("2012-05-20")
    }else {
      day1 =  as.POSIXct("2013-05-20")
    }
  } 
  
  days <- as.numeric(difftime(x,day1,units = "days"))
  return(days)
  
}

days_from_20_may <- mapply(x = trips$date_utc, y = trips$year, FUN = get.day.fun)

hist(z)

test <- cbind.data.frame(trips$date_utc, trips$year, z)

get.day.fun(as.POSIXct("2012-05-27"), "2012")



# Figure for trip types for extended period ---------
names(trips)[1] <- "trip_id"
trips_detailed <- merge.data.frame(trips,df_combined,"trip_id")

df.period <- data.frame(year= integer(48),season= character(48),
                        date_mid= .POSIXct(character(48), tz = "UTC"),p_got= numeric(48),
                        p_mix= numeric(48),p_sea= numeric(48),
                        n_got= integer(48),n_mix= integer(48),
                        n_sea= integer(48),n_tot= integer(48),
                        p_for_got_mean= numeric(48),
                        p_for_got_med = numeric(48))

str(trips)

seasons <- as.factor(c("pre_laying","incubation", "chick_early", "chick_late"))

levels(df.period$season) <- levels(seasons)


years <- 2011:2013
# For each year (3)
i <- 2
ix <- 1
for(i in 1:3){
  start_date <- as.POSIXct(paste(years[i], "-04-30 00:00:00", sep = ""), tz = "UTC")
  
  # For each date period in year (4)
  # iz  
  # iz <- 1
  for(iz in 1:16){
    
    # add 6 days to get the end date
    end_date <- start_date + 6*24*60*60
    
    # Trip filter
    tf <- (trips_detailed$date_time > start_date) & (trips_detailed$date_time < end_date)
    # summary(tf)
    
    df.period$year[ix] <- years[i]
    if(iz <= 4){
      df.period$season[ix] <- seasons[1]}else if(iz <= 8){
        df.period$season[ix] <- seasons[2]} else if(iz <=12){
          df.period$season[ix] <- seasons[3]} else {
            df.period$season[ix] <- seasons[4]}
    # df.period$season[ix] <- switch(floor(iz/4)+1, seasons[1], seasons[2], seasons[3])
    df.period$date_mid[ix] <- start_date + 3*24*60*60
    df.period$n_got[ix] <- sum(trips_detailed$class_3[tf] == "LAND", na.rm = TRUE)
    df.period$n_mix[ix] <- sum(trips_detailed$class_3[tf] == "MIX", na.rm = TRUE)
    df.period$n_sea[ix] <- sum(trips_detailed$class_3[tf] == "SEA", na.rm = TRUE)
    df.period$n_tot[ix] <- length(trips_detailed$class_3[tf])
    df.period$p_got[ix] <- df.period$n_got[ix]/df.period$n_tot[ix]
    df.period$p_mix[ix] <- df.period$n_mix[ix]/df.period$n_tot[ix]
    df.period$p_sea[ix] <- df.period$n_sea[ix]/df.period$n_tot[ix]
    df.period$p_for_got_mean[ix] <- mean(trips_detailed$p_for_got[tf], na.rm = TRUE)
    df.period$p_for_got_med[ix] <- median(trips_detailed$p_for_got[tf], na.rm = TRUE)
    ix <- ix +1
    start_date <- start_date + 5*24*60*60
  }
  
}

# New code
library(ggplot2)
library(scales)
library(reshape2)

gg_trips_period_df <- cbind.data.frame(df.period$year, df.period$date_mid,
                                       df.period$p_got*100, df.period$p_mix*100,
                                       df.period$p_sea*100)
names(gg_trips_period_df) <- c("Year", "Date", "Land", "Mixed", "Sea")
str(gg_trips_period_df)

gg_trips_period_df$Date <- as.Date(paste("2011-", format(gg_trips_period_df$Date, "%m-%d"), sep = ""))

gg_df_period_new <- melt(gg_trips_period_df, id.vars = c("Year", "Date"))

gg_df_period_new[is.na(gg_df_period_new)]<- 0

date.per <- as.numeric(c(as.Date("2011-05-20"),as.Date("2011-06-10"), as.Date("2011-07-01")))

dpi <- 1000
library(scales) # for date_breaks()
# png("ggplot_prop_land_sea_sep_fig_2.png", , width = 10*dpi, height = 5*dpi, res = dpi)
png("ggplot_prop_land_sea_mix_date_new_extended_sup_alt.png", width = 10*dpi, height = 10*dpi, res = dpi)
ggplot(gg_df_period_new, aes(x = Date, y = value, color = variable)) + 
  # scale_x_date(date_minor_breaks = "5 day") +
  geom_line(lwd = 1)+
  scale_color_manual(values=c("dark green", "magenta", "dark blue")) +
  facet_grid(Year ~ ., scale = "fixed") +
  # ylim(0, 100) +
  geom_vline(xintercept = date.per,
             colour="dark grey", linetype = "longdash", lwd =1.5) +
  scale_y_continuous(name="Proporition of trips (%)", breaks=seq(0,100,20),
                     limits = c(0,100)) + 
  # scale_x_continuous(name="Date") +
  # scale_y_continuous() +
  scale_x_date(breaks = date_breaks("10 day"),
               minor_breaks = date_breaks("5 day"),
               labels=date_format("%d-%b"))+
  # date_labels = "%d-%b") +
  # scale_x_date(date_labels = "%d-%b")+
  # ylim(0,100)+
  theme_bw() +
  theme(axis.title = element_text(face="bold", size=18),
        axis.text  = element_text(size = 12),
        legend.title = element_blank(),
        legend.text = element_text(size = 14))
# + geom_text(aes(x, y, label=labs, group=NULL),data=dat)
# annotate("text", x = (c(as.Date("2011-05-30"))), y = 90, label = "Some text")

dev.off()
# getwd()



# # All years pooled inc pre_laying ----
# 
# df.period <- NULL
# df.period <- data.frame(season= character(16),
#                         date_mid= .POSIXct(character(16), tz = "UTC"),p_got= numeric(16),
#                         p_mix= numeric(16),p_sea= numeric(16),
#                         n_got= integer(16),n_mix= integer(16),
#                         n_sea= integer(16),n_tot= integer(16),
#                         p_for_got_mean= numeric(16),
#                         p_for_got_med = numeric(16))
# 
# # str(trips)
# # levels(trips_detailed$stage)
# seasons <- as.factor(c("pre_laying", "incubation", "chick_early", "chick_late"))
# 
# levels(df.period$season) <- levels(seasons)
# 
# 
# years <- 2011:2013# For each year (3)
# # i <- 2
# # ix <- 1
# # for(i in 1:3){
# start_date <- as.POSIXct(paste(years, "-05-20 00:00:00", sep = ""), tz = "UTC")
# 
# # For each date period in year (4)
# # iz  
# # iz <- 1
# for(iz in 1:16){
#   
#   # add 6 days to get the end date
#   end_date <- start_date + 6*24*60*60
#   
#   # Trip filter
#   tf <- ((trips_detailed$date_time > start_date[1]) & (trips_detailed$date_time < end_date[1])) |
#     (trips_detailed$date_time > start_date[2]) & (trips_detailed$date_time < end_date[2]) |
#     (trips_detailed$date_time > start_date[3]) & (trips_detailed$date_time < end_date[3])
#   # summary(tf)
#   
#   # df.period$year[ix] <- years[i]
#   if(iz <= 4) {df.period$season[iz] <- seasons[1]}else if(iz <= 8){
#     df.period$season[iz] <- seasons[2]
#   } else df.period$season[iz] <- seasons[3]
#   # df.period$season[ix] <- switch(floor(iz/4)+1, seasons[1], seasons[2], seasons[3])
#   df.period$date_mid[iz] <- start_date + 3*24*60*60
#   df.period$n_got[iz] <- sum(trips_detailed$class_3[tf] == "LAND", na.rm = TRUE)
#   df.period$n_mix[iz] <- sum(trips_detailed$class_3[tf] == "MIX", na.rm = TRUE)
#   df.period$n_sea[iz] <- sum(trips_detailed$class_3[tf] == "SEA", na.rm = TRUE)
#   df.period$n_tot[iz] <- length(trips_detailed$class_3[tf])
#   df.period$p_got[iz] <- df.period$n_got[iz]/df.period$n_tot[iz]
#   df.period$p_mix[iz] <- df.period$n_mix[iz]/df.period$n_tot[iz]
#   df.period$p_sea[iz] <- df.period$n_sea[iz]/df.period$n_tot[iz]
#   df.period$p_for_got_mean[iz] <- mean(trips_detailed$p_for_got[tf], na.rm = TRUE)
#   df.period$p_for_got_med[iz] <- median(trips_detailed$p_for_got[tf], na.rm = TRUE)
#   # ix <- ix +1
#   start_date <- start_date + 5*24*60*60
# }
# 
# 
# library(ggplot2)
# library(scales)
# library(reshape2)
# 
# gg_trips_period_df <- cbind.data.frame(df.period$date_mid,
#                                        df.period$p_got*100, df.period$p_mix*100,
#                                        df.period$p_sea*100)
# names(gg_trips_period_df) <- c("Date", "Land", "Mixed", "Sea")
# str(gg_trips_period_df)
# 
# gg_trips_period_df$Date <- as.Date(paste("2011-", format(gg_trips_period_df$Date, "%m-%d"), sep = ""))
# 
# gg_df_period_new <- melt(gg_trips_period_df, id.vars = c("Date"))
# 
# date.per <- as.numeric(c(as.Date("2011-06-10"), as.Date("2011-07-01")))
# 
# dpi <- 1000
# library(scales) # for date_breaks()
# # png("ggplot_prop_land_sea_sep_fig_2.png", , width = 10*dpi, height = 5*dpi, res = dpi)
# png("ggplot_prop_land_sea_mix_date_new_allyearspooled_alt_theme.png", width = 10*dpi, height = 4*dpi, res = dpi)
# ggplot(gg_df_period_new, aes(x = Date, y = value, color = variable)) + 
#   # scale_x_date(date_minor_breaks = "5 day") +
#   geom_line(lwd = 1)+
#   scale_color_manual(values=c("dark green", "magenta", "dark blue")) +
#   # facet_grid(Year ~ ., scale = "fixed") +
#   # ylim(0, 100) +
#   geom_vline(xintercept = date.per,
#              colour="dark grey", linetype = "longdash", lwd =1.5) +
#   scale_y_continuous(name="Proporition of trips (%)", breaks=seq(0,100,20),
#                      limits = c(0,100)) + 
#   # scale_x_continuous(name="Date") +
#   # scale_y_continuous() +
#   scale_x_date(breaks = date_breaks("10 day"),
#                minor_breaks = date_breaks("5 day"),
#                labels=date_format("%d-%b"))+
#   # date_labels = "%d-%b") +
#   # scale_x_date(date_labels = "%d-%b")+
#   # ylim(0,100)+
#   theme_bw() +
#   theme(axis.title = element_text(face="bold", size=18),
#         axis.text  = element_text(size = 12),
#         legend.title = element_blank(),
#         legend.text = element_text(size = 14))
# # + geom_text(aes(x, y, label=labs, group=NULL),data=dat)
# # annotate("text", x = (c(as.Date("2011-05-30"))), y = 90, label = "Some text")
# 
# dev.off()
# 



# Figure for trip types for extended period - years pooled ---------
names(trips)[1] <- "trip_id"
trips_detailed <- merge.data.frame(trips,df_combined,"trip_id")

df.period <- data.frame(year= integer(16),season= character(16),
                        date_mid= .POSIXct(character(16), tz = "UTC"),p_got= numeric(16),
                        p_mix= numeric(16),p_sea= numeric(16),
                        n_got= integer(16),n_mix= integer(16),
                        n_sea= integer(16),n_tot= integer(16),
                        p_for_got_mean= numeric(16),
                        p_for_got_med = numeric(16))

str(trips)



seasons <- as.factor(c("pre_laying","incubation", "chick_early", "chick_late"))

levels(df.period$season) <- levels(seasons)


trips_detailed$date_time_new <-  as.POSIXct(paste(
  "2011-", format(trips_detailed$date_time, "%m-%d"), sep = ""), tz = "UTC")

# gg_trips_period_df$Date <- as.Date(paste("2011-", format(gg_trips_period_df$Date, "%m-%d"), sep = ""))

# years <- 2011:2013
# For each year (3)
# i <- 2
ix <- 1
for(i in 1:1){
  start_date <- as.POSIXct(paste(2011, "-04-30 00:00:00", sep = ""), tz = "UTC")
  
  # For each date period in year (4)
  # iz  
  # iz <- 1
  for(iz in 1:16){
    
    # add 6 days to get the end date
    end_date <- start_date + 6*24*60*60
    
    # Trip filter
    tf <- (trips_detailed$date_time_new > start_date) & (trips_detailed$date_time_new < end_date)
    # summary(tf)
    
    df.period$year[ix] <- years[i]
    if(iz <= 4){
      df.period$season[ix] <- seasons[1]}else if(iz <= 8){
        df.period$season[ix] <- seasons[2]} else if(iz <=12){
          df.period$season[ix] <- seasons[3]} else {
            df.period$season[ix] <- seasons[4]}
    # df.period$season[ix] <- switch(floor(iz/4)+1, seasons[1], seasons[2], seasons[3])
    df.period$date_mid[ix] <- as.Date(start_date + 3*24*60*60)
    df.period$n_got[ix] <- sum(trips_detailed$class_3[tf] == "LAND", na.rm = TRUE)
    df.period$n_mix[ix] <- sum(trips_detailed$class_3[tf] == "MIX", na.rm = TRUE)
    df.period$n_sea[ix] <- sum(trips_detailed$class_3[tf] == "SEA", na.rm = TRUE)
    df.period$n_tot[ix] <- length(trips_detailed$class_3[tf])
    df.period$p_got[ix] <- df.period$n_got[ix]/df.period$n_tot[ix]
    df.period$p_mix[ix] <- df.period$n_mix[ix]/df.period$n_tot[ix]
    df.period$p_sea[ix] <- df.period$n_sea[ix]/df.period$n_tot[ix]
    df.period$p_for_got_mean[ix] <- mean(trips_detailed$p_for_got[tf], na.rm = TRUE)
    df.period$p_for_got_med[ix] <- median(trips_detailed$p_for_got[tf], na.rm = TRUE)
    ix <- ix +1
    start_date <- start_date + 5*24*60*60
  }
  
}


df.period$date_mid <- as.Date(paste("2011-", format(df.period$date_mid, "%m-%d"), sep = ""))

# New code
library(ggplot2)
library(scales)
library(reshape2)

gg_trips_period_df <- cbind.data.frame(df.period$date_mid,
                                       df.period$p_got*100, df.period$p_mix*100,
                                       df.period$p_sea*100)
names(gg_trips_period_df) <- c("Date", "Land", "Mixed", "Sea")
str(gg_trips_period_df)


gg_df_period_new <- melt(gg_trips_period_df, id.vars = c("Date"))
gg_df_period_new[is.na(gg_df_period_new)]<- 0
str(gg_df_period_new)

date.per <- as.numeric(c(as.Date("2011-05-20"),as.Date("2011-06-10"), as.Date("2011-07-01")))

dpi <- 1000
library(scales) # for date_breaks()
# png("ggplot_prop_land_sea_sep_fig_2.png", , width = 10*dpi, height = 5*dpi, res = dpi)
png("ggplot_prop_land_sea_mix_date_new_extended_4a_NEW.png", width = 10*dpi, height = 6*dpi, res = dpi)
ggplot(gg_df_period_new, aes(x = Date, y = value, color = variable)) + 
  # scale_x_date(date_minor_breaks = "5 day") +
  geom_line(lwd = 1)+
  scale_color_manual(values=c("dark green", "magenta", "dark blue")) +
  # facet_grid(Year ~ ., scale = "fixed") +
  # ylim(0, 100) +
  geom_vline(xintercept = date.per,
             colour="dark grey", linetype = "longdash", lwd =1.5) +
  scale_y_continuous(name="Proporition of trips (%)", breaks=seq(0,100,20),
                     limits = c(0,100)) + 
  # scale_x_continuous(name="Date") +
  # scale_y_continuous() +
  scale_x_date(breaks = date_breaks("10 day"),
               minor_breaks = date_breaks("5 day"),
               labels=date_format("%d-%b"))+
  # date_labels = "%d-%b") +
  # scale_x_date(date_labels = "%d-%b")+
  # ylim(0,100)+
  theme_bw() +
  theme(axis.title = element_text(face="bold", size=18),
        axis.text  = element_text(size = 12),
        legend.title = element_blank(),
        legend.text = element_text(size = 16),
        legend.position=c(.9, .5))
# + geom_text(aes(x, y, label=labs, group=NULL),data=dat)
# annotate("text", x = (c(as.Date("2011-05-30"))), y = 90, label = "Some text")

dev.off()
# getwd()


# plot trip classification figure ------
load("trip_summary_data.RData")

source("plot_trips_fun.R")



f <- (p_gotland_over_3km_1ms_sub3move >= 0.2 & p_gotland_over_3km_1ms_sub3move <= 0.8)

f2 <- trip.points.new$col_dist > 3000 & trip.points.new$p2p_dist < 1500 &
  trip.points.new$p2p_dist > 20  & trip.points.new$gotland_on_bool

f3 <- trip.points.new$col_dist > 3000 & trip.points.new$p2p_dist < 1500 &
  trip.points.new$p2p_dist > 20  & !trip.points.new$gotland_on_bool

# marine trips
# 2728 2559 2342 2000 3308 1639 3113  347 3116 1305

# Land trips
# 54 2449   79 2245 1393 3259 1368  710  500   24
# 1592 1441  272 1726  957  476 1591  986 2353  260
# 2719 1357 1968 1573 2091 1361 1439 1394 1564 2266
trip_ids <- unique(trips$trip_id_int[f])

# inspect.trips <- trips[f,]
# Mixed (range 20 - 80 %)
# 2757 2241 2275 1320   12 3264 1760 1754 1365 2953

# If too many trips to plot, plot a sample
# if(length(trip_ids > 8)){
# trip_ids <- sample(trip_ids,10)
# }
trip_id <- trip_ids[60]

points.f <- trip.points.new$trip_id == trip_id
# summary(points.f)
for_points_got <- (f2 & points.f)[points.f]
for_points_sea <- (f3 & points.f)[points.f]
points_other <- (!f2 & !f3 & points.f)[points.f]

# summary(for_points)
dpi = 1000
png("example_trips_land_0.2_0.8_mixed_example_gotland.png", width = 6*dpi, height = 6*dpi, res = dpi)
cairo_ps(filename = "example_trips_land_0.2_0.8_mixed_example_gotland.ps", width = 6, height = 6)
# pdf("example_trips_sea_0.05less_4.pdf", width = 5, height = 8)
# postscript("example_trips_sea_0.05less_4.eps", width = 5, height = 8, horizontal = FALSE, onefile = FALSE)
# library(devEMF)
# emf(file="example_trips_new.emf", bg="white", width=5, height=8, family="Calibri", pointsize=20)
# win.metafile(filename = "example_trips_sea_0.05less_4.wmf", width = 5, height = 8, pointsize = 12,
#              restoreConsole = TRUE)
plot.trips(long = trip.points.new$longitude[points.f],
           lat = trip.points.new$latitude[points.f],
           trip_ids = trip.points.new$trip_id[points.f],
           for_points_got =  for_points_got,
           for_points_sea =  for_points_sea,
           other_points = points_other,
           gotland_poly = TRUE)

dev.off()
# plot(1:10)
