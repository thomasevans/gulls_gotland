# Plot maps of trips



# Load foraging trip info ------
load("GPS_LBBG1.Rdata")



# random representative selection ------
uniques <- unique.array(cbind.data.frame(trips_f$device_info_serial,
                   trips_f$year))
# Shuffle order
uniques <- uniques[sample(seq_along(uniques[,1])),]

n <- length(uniques[,1])

# 
# hist(trips_f$gotland_time_prop[gotland_on == TRUE])
# hist(trips_f$gotland_time_prop[gotland_on == FALSE])

land_10 <- NULL

count <- 1
i <- 1
while(count < 11){
  if(i > n){i <- 1}  # Reset i if we get to the end of the list
  
  t.sel <- as.character(trips_f$trip_id[trips_f$device_info_serial == uniques[i,1] &
                             trips_f$year == uniques[i,2] &
                             gotland_on == TRUE])
  str(t.sel)
  if(length(t.sel) > 5){
    land_10[count] <- sample(t.sel,1)
    count <- count + 1
  }
  i <- i + 1  
}
str(land_10)

# trips.l <- trips_f[ trips_f$trip_id  %in%
                      # as.character(land_10),]
# str(trips_f$trip_id )
# summary(trips_f$trip_id %in% land_10)


sea_10 <- NULL

count <- 1
i <- 1
while(count < 11){
  if(i > n){i <- 1}  # Reset i if we get to the end of the list
  
  t.sel <- as.character(trips_f$trip_id[trips_f$device_info_serial == uniques[i,1] &
                                          trips_f$year == uniques[i,2] &
                                          gotland_on == FALSE])
  if(length(t.sel) > 5){
    sea_10[count] <- sample(t.sel,1)
    count <- count + 1
  }
  i <- i + 1  
}

sea_10
#  [1]  256  744 1013  538  169 1209  904  773  397   31
# [1] "511"  "2410" "370"  "2734" "2502" "347"  "3117" "2651" "642"  "1419"

land_10
#  [1]  579  259  738 1030  542  192  912  774  367   48
# [1] "2475" "2824" "2587" "346"  "648"  "1386" "267"  "1987" "1611" "2929"


# Select 10 land based -----
# land_10 <- sample(trips_f$trip_id[gotland_on == TRUE], 10)
# land_10 <- c("1571", "54", "439","1394", "949",  "1355", "1395", "2761", "2072", "3257")
# 
# sea_10 <- sample(trips_f$trip_id[gotland_on == FALSE], 10)


# Get GPS data -----
# DB package
library("RODBC")

gps.db2 <- odbcConnectAccess2007('D:/Dropbox/tracking_db/GPS_db.accdb')

# All trips
gps <- sqlQuery(gps.db2, query="SELECT gps_uva_tracking_speed_3d_limited.device_info_serial, gps_uva_tracking_speed_3d_limited.date_time, gps_uva_tracking_speed_3d_limited.latitude, gps_uva_tracking_speed_3d_limited.longitude, gps_uva_tracking_speed_3d_limited.speed_3d, lund_trips.trip_id
FROM gps_uva_tracking_speed_3d_limited, lund_trips
                WHERE (((gps_uva_tracking_speed_3d_limited.date_time)>=[lund_trips].[start_time] And (gps_uva_tracking_speed_3d_limited.date_time)<=[lund_trips].[end_time]) AND ((lund_trips.device_info_serial)=[gps_uva_tracking_speed_3d_limited].[device_info_serial]))
                ORDER BY gps_uva_tracking_speed_3d_limited.device_info_serial, gps_uva_tracking_speed_3d_limited.date_time;
                ")

# Save GPS data as binnary object for future retrieval.
save(gps, file = "gps_trip_data.RData")
load("gps_trip_data.RData")


gps <- na.omit(gps)

# test <- gps[gps$trip_id == land_10[1],]


# Alpha channel ----
addalpha <- function(colors, alpha=1.0) {
  r <- col2rgb(colors, alpha=T)
  # Apply alpha
  r[4,] <- alpha*255
  r <- r/255.0
  return(rgb(r[1,], r[2,], r[3,], r[4,]))
}


# Function to plot a base map ----
# Plot a map with coastlines only - no 
library(maps)
library(RColorBrewer)

# Coastline data
load("SWE_adm0.RData")


map.base.fun <- function(xlim = c(17,18.3), ylim =  c(57,57.7)){
  par(mfrow=c(1,1))
  par( mar = c(5, 4, 4, 2))
  #   par(bg = 'white')
  
  # col.green <- brewer.pal(5,"Greens")
  # col.blue <- brewer.pal(5,"Blues")
  
  
  plot(gadm, xlim = xlim,
       ylim = ylim, col= "grey40", bg = "white",
       lty = 0)
  
  ## Scale bar and axis
  box(col="black",lwd=2)
  axis(side=(1),las=1,col="black",col.axis="black")
  axis(side=(2),las=1,col="black",col.axis="black")
  
}


# hack map.scale function
# map.scale2 <- map.scale
# fix(map.scale2)
# source("D:/Dropbox/R_projects/murre_gps/map.scale2.R")

# Plot trips ----

library(RColorBrewer)
# lbbg_flight_ids <- unique(gps_lbbg$flight_id)
# col.vec <- rainbow(length(land_10))
col.vec <- brewer.pal(10, "Paired")


col.vec.al <- addalpha(col.vec, alpha = 0.7)
col.vec.al.rand <- col.vec.al[sample(seq_along(col.vec.al))]


# Plot all land trips -------
resa = 72*4
png("trips_land_test.png", res = resa, width = 8*resa, height = 8*resa)
# ?png
# i <- 12
gps.s <- gps[gps$trip_id %in% land_10,]

xlim.n <- range(gps.s$longitude)
ylim.n <- range(gps.s$latitude)

x.range <- xlim.n[2] - xlim.n[1]

xlim.n <- c((xlim.n[1]-0.1*x.range), (xlim.n[2]+0.1*x.range))

y.range <- ylim.n[2] - ylim.n[1]

ylim.n <- c((ylim.n[1]-0.1*y.range), (ylim.n[2]+0.1*y.range))


map.base.fun(xlim = xlim.n, ylim = ylim.n)
i <- 1
for(i in 1:length(land_10)){
  
  x <- land_10[i]
  # ?subset
  gps.sub <- gps.s[gps.s$trip_id == x,]
  n <- length(gps.sub$longitude)
  segments(gps.sub$longitude[-1], gps.sub$latitude[-1],
           gps.sub$longitude[1:n-1], gps.sub$latitude[1:n-1],
           col = col.vec.al.rand[i], lty = 1, lwd = 2)
}
map.scale(ratio = FALSE,
           relwidth = 0.25, cex = 1.2)
dev.off()





# Plot all sea trips -------
resa = 72*4
png("trips_sea_test.png", res = resa, width = 8*resa, height = 8*resa)
# ?png
# i <- 12
gps.s <- gps[gps$trip_id %in% sea_10,]

xlim.n <- range(gps.s$longitude)
ylim.n <- range(gps.s$latitude)

x.range <- xlim.n[2] - xlim.n[1]

xlim.n <- c((xlim.n[1]-0.1*x.range), (xlim.n[2]+0.1*x.range))

y.range <- ylim.n[2] - ylim.n[1]

ylim.n <- c((ylim.n[1]-0.1*y.range), (ylim.n[2]+0.1*y.range))


map.base.fun(xlim = xlim.n, ylim = ylim.n)

for(i in 1:length(sea_10)){
  
  x <- sea_10[i]
  # ?subset
  gps.sub <- gps.s[gps.s$trip_id == x,]
  n <- length(gps.sub$longitude)
  segments(gps.sub$longitude[-1], gps.sub$latitude[-1],
           gps.sub$longitude[1:n-1], gps.sub$latitude[1:n-1],
           col = col.vec.al.rand[i], lty = 1, lwd = 2)
}
map.scale(ratio = FALSE,
          relwidth = 0.25, cex = 1.2)
dev.off()


# trips_f_map <- rbind.data.frame(
#   cbind.data.frame(trips_f[land_10,],"land"),
#   cbind.data.frame(trips_f[sea_10,],"sea")
# )
x <- cbind.data.frame(trips_f[trips_f$trip_id %in% land_10,],"land")
y <- cbind.data.frame(trips_f[trips_f$trip_id %in% sea_10,],"sea")
names(x)[50] <- "type"
names(y)[50] <- "type"
trips_f_map <- rbind.data.frame(x,y)

write.table(trips_f_map, file = "mapped_trips.csv",
            sep = ",")
