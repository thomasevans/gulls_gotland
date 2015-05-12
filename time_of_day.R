# This script calculates time since sunrise, and calculates a variable based on that, plus specifies whether a trip is morning or afternoon (UTC).


# Packages + data ----
# install.packages("solaR")
library("solaR")


# Load trip data
trips <- read.csv("foraging_trip_info_filtered_may2015_start_time_sunrise_only.csv", header = TRUE)


# Check date_time format
date_time_start <- as.POSIXct(trips$start_time, tz = "utc")
date_time_sunrise <- as.POSIXct(trips$sunrise_date_time, tz = "utc")


# Morning or evening? (according to solar time) --------

# Convert start time to local solar time
date_time_start_solar <- local2Solar(date_time_start, lon = 17.97)

# See how this looks
date_time_start_solar[1:5]
date_time_start[1:5]



# Extract the hour from the time
date_time_start.hour <- as.numeric(format(date_time_start_solar, "%H"))
# hist(date_time_start.hour)
# Label as AM or PM
# Empty variable
am.pm <- NULL

# If you have a vector (list) of values, test this for all of these
am.pm.fun <- function(x){
  if(x < 12) am.pm <- "AM" else am.pm <- "PM"
  return(am.pm)
}

am.pm <- sapply(date_time_start.hour, am.pm.fun)
am.pm <- as.factor(am.pm)

# Number of trips leaving in morning or afternoon
summary(am.pm)


# Time since sunrise   --------
# (cos(π*Hours since sunrise/12))


# Calculate time since sunrise
time_since_sunrise <- difftime(date_time_start,  date_time_sunrise, units = "hours")
time_since_sunrise <- as.numeric(time_since_sunrise)
str(time_since_sunrise)
hist(time_since_sunrise, col = "grey")



# Double plot this data (i.e. plot it twice, as time before, and time after sunrise)
# Time before
range.fun <- function(x){
  x <- x-24
  if(x < -24){x <- (x+24)}
  return(x)
}

# Time after
range.fun.2 <- function(x){
  if(x < 0){x <- (x+24)}
  return(x)
}

x2 <- sapply(time_since_sunrise, range.fun)
x3 <- sapply(time_since_sunrise,range.fun.2)
hist(x2)
hist(x3)
# hist(time_since_sunrise)
pdf("dept_time_sunrise.pdf")
hist(rbind(x3, x2), xlim = c(-24,24), breaks = 48,
     col = "grey", xlab = "Time since/before sunrise at departure time",
     main = "Departure time relative to sunrise time (double plot)",
     xaxt = "n")
axis(1, at = seq(-24, 24, by = 4), las = 1)
dev.off()


# Calculate cos thing
time_since_sunrise.cos <-  (cos(pi*time_since_sunrise/12))

hist(time_since_sunrise.cos)

# Double plot this agains time since/ before sunrise
plot(rbind(time_since_sunrise.cos, time_since_sunrise.cos)~rbind(x3, x2))



# Output data to dataframe -----
# Put data together and give sensible column names
out.tab <- cbind.data.frame(trips$trip_id, date_time_start_solar,
                 am.pm, time_since_sunrise,  time_since_sunrise.cos)
names(out.tab) <- c("trip_id", "date_time_start_solar",
                    "time_of_day", "time_since_sunrise_h",
                    "time_since_sunrise_cos")

str(out.tab)

# Output data frame (table)
write.csv(out.tab, file = "time_of_day.data.csv", 
          row.names = FALSE)
