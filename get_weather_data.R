
# Load required packages
library("RNCEP")

# Read package documentation for 'RNCEP'
help("RNCEP")

# Or look at the website
# https://sites.google.com/site/michaelukemp/rncep

# Weather data (we use 'NCEP-DOE Reanalysis 2', option 'reanalysis2 = TRUE')

# Load trip data
trips <- read.csv("foraging_trip_info_filtered_may2015_start_time_only.csv", header = TRUE)


# Check date_time format
date_time <- as.POSIXct(trips$start_time, tz = "utc")


# Weather point location (Stora Karlsö centre)
sk_location <- NULL
sk_location$long <- 17.97
sk_location$lat  <- 57.28

i <- 1
# For each flight do following
# for(i in 1:length(trips$trip_id)){
  
  
  # Get weather data at following time points:
    # for weather variables:
    #' Wind - S-N  (‘uwnd.sig995’ U-Wind Component [East/West] (Near Surface) m/s)
   x <-    NCEP.interp("uwnd.sig995", "surface", sk_location$lat, sk_location$long,
                  date_time[1], reanalysis2 = TRUE,
                  interpolate.space = TRUE, interpolate.time = TRUE,
                  keep.unpacking.info = TRUE, return.units = TRUE,
                  interp = 'linear', p = 1, status.bar = FALSE)
        
    
    #' Wind - W-E  (‘vwnd.sig995’ V-Wind Component [North/South] (Near Surface) m/s)
    #' Temp
    #' Precipitation
    #' Cloud cover
  
    
  
  
    # t = 0
  

    
    # t = -6 h

    # t = -12 h

    # t = -18 h

  # Calculate mean/ sums for each variable
# }

# Summarise to data frame

# Output data frame (table)


