# This script gets summaries for weather variables for foraging trips. We get a summary of the prior 24 hours for each foraging trip and time of departure.


# Packages + data ----
# Load required packages
library("RNCEP")

# Read package documentation for 'RNCEP'
help("RNCEP")

# Or look at the website
# https://sites.google.com/site/michaelukemp/rncep

# Weather data (we use 'NCEP-DOE Reanalysis 2', option 'reanalysis2 = TRUE')

# Load trip data
trips <- read.csv("foraging_trip_info_filtered_july2015_start_time_only.csv", header = TRUE)


# Check date_time format
date_time <- as.POSIXct(trips$start_time, tz = "utc")


# Weather point location (Stora Karlsö centre)
sk_location <- NULL
sk_location$long <- 17.97
sk_location$lat  <- 57.28



# Get weather data -------
# WARNING - SLOW!
  
# 6 hour time difference, to be used to get data at 6h, 12h, and 18h before start point
hours_6 <- as.difftime(6, units = "hours")


  # Get weather data at following time points:
    # for weather variables:
    #' Wind - S-N  (‘uwnd.sig995’ U-Wind Component [East/West] (Near Surface) m/s)


uwnd.10m.0h <-    NCEP.interp("uwnd.10m", "gaussian", sk_location$lat, sk_location$long,
                              date_time, reanalysis2 = TRUE,
                              interpolate.space = TRUE, interpolate.time = TRUE,
                              keep.unpacking.info = TRUE, return.units = TRUE,
                              interp = 'linear', p = 1, status.bar = FALSE)

uwnd.10m.6h <-    NCEP.interp("uwnd.10m", "gaussian", sk_location$lat, sk_location$long,
                              (date_time-hours_6), reanalysis2 = TRUE,
                              interpolate.space = TRUE, interpolate.time = TRUE,
                              keep.unpacking.info = TRUE, return.units = TRUE,
                              interp = 'linear', p = 1, status.bar = FALSE)

uwnd.10m.12h <-    NCEP.interp("uwnd.10m", "gaussian", sk_location$lat, sk_location$long,
                              (date_time-(hours_6 * 2)), reanalysis2 = TRUE,
                              interpolate.space = TRUE, interpolate.time = TRUE,
                              keep.unpacking.info = TRUE, return.units = TRUE,
                              interp = 'linear', p = 1, status.bar = FALSE)

uwnd.10m.18h <-    NCEP.interp("uwnd.10m", "gaussian", sk_location$lat, sk_location$long,
                               (date_time-(hours_6 * 2)), reanalysis2 = TRUE,
                               interpolate.space = TRUE, interpolate.time = TRUE,
                               keep.unpacking.info = TRUE, return.units = TRUE,
                               interp = 'linear', p = 1, status.bar = FALSE)



    #' Wind - W-E  (‘vwnd.sig995’ V-Wind Component [North/South] (Near Surface) m/s)
vwnd.10m.0h <-    NCEP.interp("vwnd.10m", "gaussian", sk_location$lat, sk_location$long,
                              date_time, reanalysis2 = TRUE,
                              interpolate.space = TRUE, interpolate.time = TRUE,
                              keep.unpacking.info = TRUE, return.units = TRUE,
                              interp = 'linear', p = 1, status.bar = FALSE)

vwnd.10m.6h <-    NCEP.interp("vwnd.10m", "gaussian", sk_location$lat, sk_location$long,
                              (date_time-hours_6), reanalysis2 = TRUE,
                              interpolate.space = TRUE, interpolate.time = TRUE,
                              keep.unpacking.info = TRUE, return.units = TRUE,
                              interp = 'linear', p = 1, status.bar = FALSE)

vwnd.10m.12h <-    NCEP.interp("vwnd.10m", "gaussian", sk_location$lat, sk_location$long,
                               (date_time-(hours_6 * 2)), reanalysis2 = TRUE,
                               interpolate.space = TRUE, interpolate.time = TRUE,
                               keep.unpacking.info = TRUE, return.units = TRUE,
                               interp = 'linear', p = 1, status.bar = FALSE)

vwnd.10m.18h <-    NCEP.interp("vwnd.10m", "gaussian", sk_location$lat, sk_location$long,
                               (date_time-(hours_6 * 2)), reanalysis2 = TRUE,
                               interpolate.space = TRUE, interpolate.time = TRUE,
                               keep.unpacking.info = TRUE, return.units = TRUE,
                               interp = 'linear', p = 1, status.bar = FALSE)

    
    
    #' Temp air.2m
air.2m.0h <-    NCEP.interp("air.2m", "gaussian", sk_location$lat, sk_location$long,
                              date_time, reanalysis2 = TRUE,
                              interpolate.space = TRUE, interpolate.time = TRUE,
                              keep.unpacking.info = TRUE, return.units = TRUE,
                              interp = 'linear', p = 1, status.bar = FALSE)

air.2m.6h <-    NCEP.interp("air.2m", "gaussian", sk_location$lat, sk_location$long,
                              (date_time-hours_6), reanalysis2 = TRUE,
                              interpolate.space = TRUE, interpolate.time = TRUE,
                              keep.unpacking.info = TRUE, return.units = TRUE,
                              interp = 'linear', p = 1, status.bar = FALSE)

air.2m.12h <-    NCEP.interp("air.2m", "gaussian", sk_location$lat, sk_location$long,
                               (date_time-(hours_6 * 2)), reanalysis2 = TRUE,
                               interpolate.space = TRUE, interpolate.time = TRUE,
                               keep.unpacking.info = TRUE, return.units = TRUE,
                               interp = 'linear', p = 1, status.bar = FALSE)

air.2m.18h <-    NCEP.interp("air.2m", "gaussian", sk_location$lat, sk_location$long,
                               (date_time-(hours_6 * 2)), reanalysis2 = TRUE,
                               interpolate.space = TRUE, interpolate.time = TRUE,
                               keep.unpacking.info = TRUE, return.units = TRUE,
                               interp = 'linear', p = 1, status.bar = FALSE)
    
    
    
    #' Precipitation
prate.sfc.0h <-    NCEP.interp("prate.sfc", "gaussian", sk_location$lat, sk_location$long,
                            date_time, reanalysis2 = TRUE,
                            interpolate.space = TRUE, interpolate.time = TRUE,
                            keep.unpacking.info = TRUE, return.units = TRUE,
                            interp = 'linear', p = 1, status.bar = FALSE)

prate.sfc.6h <-    NCEP.interp("prate.sfc", "gaussian", sk_location$lat, sk_location$long,
                            (date_time-hours_6), reanalysis2 = TRUE,
                            interpolate.space = TRUE, interpolate.time = TRUE,
                            keep.unpacking.info = TRUE, return.units = TRUE,
                            interp = 'linear', p = 1, status.bar = FALSE)

prate.sfc.12h <-    NCEP.interp("prate.sfc", "gaussian", sk_location$lat, sk_location$long,
                             (date_time-(hours_6 * 2)), reanalysis2 = TRUE,
                             interpolate.space = TRUE, interpolate.time = TRUE,
                             keep.unpacking.info = TRUE, return.units = TRUE,
                             interp = 'linear', p = 1, status.bar = FALSE)

prate.sfc.18h <-    NCEP.interp("prate.sfc", "gaussian", sk_location$lat, sk_location$long,
                             (date_time-(hours_6 * 2)), reanalysis2 = TRUE,
                             interpolate.space = TRUE, interpolate.time = TRUE,
                             keep.unpacking.info = TRUE, return.units = TRUE,
                             interp = 'linear', p = 1, status.bar = FALSE)    
    
    
    #' Cloud cover   tcdc.eatm
tcdc.eatm.0h <-    NCEP.interp("tcdc.eatm", "gaussian", sk_location$lat, sk_location$long,
                               date_time, reanalysis2 = TRUE,
                               interpolate.space = TRUE, interpolate.time = TRUE,
                               keep.unpacking.info = TRUE, return.units = TRUE,
                               interp = 'linear', p = 1, status.bar = FALSE)

tcdc.eatm.6h <-    NCEP.interp("tcdc.eatm", "gaussian", sk_location$lat, sk_location$long,
                               (date_time-hours_6), reanalysis2 = TRUE,
                               interpolate.space = TRUE, interpolate.time = TRUE,
                               keep.unpacking.info = TRUE, return.units = TRUE,
                               interp = 'linear', p = 1, status.bar = FALSE)

tcdc.eatm.12h <-    NCEP.interp("tcdc.eatm", "gaussian", sk_location$lat, sk_location$long,
                                (date_time-(hours_6 * 2)), reanalysis2 = TRUE,
                                interpolate.space = TRUE, interpolate.time = TRUE,
                                keep.unpacking.info = TRUE, return.units = TRUE,
                                interp = 'linear', p = 1, status.bar = FALSE)

tcdc.eatm.18h <-    NCEP.interp("tcdc.eatm", "gaussian", sk_location$lat, sk_location$long,
                                (date_time-(hours_6 * 2)), reanalysis2 = TRUE,
                                interpolate.space = TRUE, interpolate.time = TRUE,
                                keep.unpacking.info = TRUE, return.units = TRUE,
                                interp = 'linear', p = 1, status.bar = FALSE)    



# Summarise variables (mean/ sum) --------

  # Calculate mean/ sums for each variable


tcdc.eatm.mean <- rowMeans(cbind(tcdc.eatm.0h, tcdc.eatm.6h,
                                 tcdc.eatm.12h, tcdc.eatm.18h), na.rm = TRUE)

prate.sfc.sum <-  rowSums(cbind(prate.sfc.0h, prate.sfc.6h,
                                 prate.sfc.12h, prate.sfc.18h), na.rm = TRUE)

air.2m.mean <-  rowMeans(cbind(air.2m.0h, air.2m.6h,
                               air.2m.12h, air.2m.18h), na.rm = TRUE)


vwnd.10m.mean <-   rowMeans(cbind(vwnd.10m.0h, vwnd.10m.6h,
                                  vwnd.10m.12h, vwnd.10m.18h), na.rm = TRUE)

uwnd.10m.mean <-   rowMeans(cbind(uwnd.10m.0h, uwnd.10m.6h,
                                  uwnd.10m.12h, uwnd.10m.18h), na.rm = TRUE)

# Convert from Kelvin to Celsius 
air.2m.mean.c <- air.2m.mean - 273.15


# Histograms of variables (to visualise distributions) ------
hist(tcdc.eatm.mean)
hist(prate.sfc.sum)
hist(air.2m.mean.c)
hist(vwnd.10m.mean)
hist(uwnd.10m.mean)

# Rain per 24h (in mm)
hist(prate.sfc.sum*6*60*60, breaks = 100)



# Output data to new table (csv file) -------

# Summarise to data frame
weather.data <- cbind.data.frame(trips$trip_id,tcdc.eatm.mean,
                                 prate.sfc.sum, air.2m.mean.c,
                                 vwnd.10m.mean, uwnd.10m.mean)
names(weather.data)[1] <- "trip_id"
str(weather.data)


# Output data frame (table)
write.csv(weather.data, file = "weather.data.csv", 
          row.names = FALSE)

