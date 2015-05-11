
tod <- seq(0,24,0.1)

light_lev <- cos((pi*tod/12))
all_day <-  cos((pi*tod/24))

plot(light_lev~tod)
points(all_day~tod, col = "red")

?hour

?switch


# Make an example date-time object
x <- as.POSIXct(c("2014-05-02 12:45:00",
                  "2014-05-02 10:45:00",
                  "2014-05-02 19:45:00"), tz = "UTC")

# Extract the hour from the time
x.hour <- as.numeric(format(x, "%H"))

# Label as AM or PM
# Empty variable
am.pm <- NULL

# If before midday, label AM, else label PM
if(x < 12) am.pm <- "AM" else am.pm <- "PM"

# If you have a vector (list) of values, test this for all of these
am.pm.fun <- function(x){
  if(x < 12) am.pm <- "AM" else am.pm <- "PM"
  return(am.pm)
}

am.pm.thing <- sapply(x.hour, am.pm.fun)





# Time sinse sunrise
# (cos(π*Hours since sunrise/12))

# Make example vector of sunrise times
# Below are examples, not true sunrise times
sunrise_times <- as.POSIXct(c("2014-05-02 05:45:00",
                                   "2014-05-06 05:38:00",
                                   "2014-08-10 06:04:00"), tz = "UTC")

# Example trip departure times (NB sunrise time #1 above corresponds to trip departure time #1 below)
trip_dept_time <-   as.POSIXct(c("2014-05-02 03:37:00",
                                 "2014-05-06 11:54:00",
                                 "2014-08-10 18:54:00"), tz = "UTC")



# Calculate time sinse sunrise
x <- difftime(trip_dept_time,  sunrise_times, units = "hours")
hours.since.sunrise <- as.numeric(x)


# Calculate cos thing
time.since.sunrise.cos <-  (cos(pi*hours.since.sunrise/12))



