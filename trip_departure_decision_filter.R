# Primarily developed by Tom Evans at Lund University: tom.evans@biol.lu.se
# You are welcome to use parts of this code, but please give credit when using it extensively.


# Explanation text -----
# This is script is to filter the trip_departure_decision
# data to retain only relevant trips. For example it will
# only include trips of a minimum time duration and distance
# plus will exclude trips of too great a time duration.
# Further it filters by periods, so we only retain data
# for relevant months to our analysis (May, June, July),
# months during the core breeding season.

# Read in data -----
# First read in the data-table
# Best to use the file ending 'RData', as this is already
# an R object, so you shouldn't need to change data types
# etc.

# Set the data directory where the data resides
# Remember that R doesn't like back-slashes, either
# replace with forward-slashes(/), or a doulble back-slash (\\)
# setwd("D:/Dropbox/LBBG_terrestrial_foraging/departure_decision")

# Now read in the data file
load("foraging_trip_info.RData")

# After reading this file in a new object 'trips' should
# be available in your workspace

# Inspect data ----
# Check that this looks right
# See vairables and their data types
str(trips)

# See table itself (note upper case V in view)
View(trips)

# Filter data ----

# *f0 - trip_type ----
# 1/0, 1 - migratory, 0 - non-migratory
# First filter out migratory trips.
# Note double equals sign to indicate 'is equal to'
f0 <- trips$trip_type == 0
# 8 trips are now excluded 'FALSE'
summary(f0)

# Keep a running tally of all trip filters:
f <- f0


# *f1 - fix_n ----
# Fix number - Number of GPS fixes for a trip
# First see the distribution and don't include
# migratory trips (use the filter we made above)
hist(trips$fix_n[f])

# Just to see trips with relatively few GPS locations
hist(trips$fix_n[f & trips$fix_n < 1000])
hist(trips$fix_n[f & trips$fix_n < 100])
hist(trips$fix_n[f & trips$fix_n < 15])


summary(trips$fix_n >= 10)

# There's no obvious cut-off (bi-modal distribution etc)
# Therefor I suggest just using a 'reasonable' value,
# perhaps 10 fixes, which should be adequate for this
# analysis where we want to know things like - did they go
# to Gotland, aproximatly how much time on Gotland. What
# was the furthest point they reached?

# Create filter to exclude trips with less than 8 points
# Retain trips with 10 or more GPS locations (fixes)
f1 <- trips$fix_n >= 10
summary(f1)

# We can combine with the previous filter using 'AND'
# operator (&)
f <- f0 & f1
summary(f)


# *f3 - duration_s ----
# The duration of a trip in seconds
# For a trip to be a 'foraging trip' it should have
# some minimum duration, plus also a maximum.
# We only want to include 'central-place foraging'
# originating from the breeding colony at Stora
# Karlsö.

# First have a look at the data to get an idea of what
# might be sensible thresholds.

# Maximum duration
hist(trips$duration_s[f])

# One-week is almost certainly too much, but we can have a
# look at durations of trips of one-week or less.
# First calculate 1-week in seconds
# 7 days, 24 hours, 60 mins, 60 s
week <- 7 * 24 * 60 * 60
day <- 24 * 60 * 60
hist(trips$duration_s[f & trips$duration_s < week])
# Add lines for days
abline(v = c(day, day *2, day * 3, day *4, day *5),
       lty = 2, lwd = 2, col = "red")

day_h <- day/((60*60))
# View 5-days
hist((trips$duration_s[f & trips$duration_s < 5*day]/(60*60)),
     ylim = c(0,200), breaks = 40,
     ylab = "N trips", xlab = "Duration of trip (h)")
# Add lines for days
abline(v = c(day_h, day_h *2, day_h * 3, day_h *4, day_h *5),
       lty = 2, lwd = 2, col = "red")

win.metafile("trip_duration_filter.wmf", width = 14, height = 7)
par(mfrow=c(1,2))
hist((trips$duration_s[f]/(60*60)), breaks = 40,
     ylab = "N trips", xlab = "Duration of trip (h)",
     main = "All trips")
# Add lines for days
# abline(v = c(day_h, day_h *2, day_h * 3, day_h *4, day_h *5),
#        lty = 2, lwd = 2, col = "red")

hist((trips$duration_s[f & trips$duration_s < 5*day]/(60*60)), breaks = 40,
     ylab = "N trips", xlab = "Duration of trip (h)",
     main = "Trips <5 days")
# Add lines for days
abline(v = c(day_h, day_h *2, day_h * 3, day_h *4, day_h *5),
       lty = 2, lwd = 2, col = "dark grey")
abline(v = c(day_h *2),
       lty = 2, lwd = 2, col = "red")
dev.off()

# 110000 appears a potentially good threshold
abline(v = 110000,
       lty = 2, lwd = 2, col = "blue")
# What is this in days?
110000/day

summary(trips$duration_s < 110000)
summary(trips$duration_s[f] < 2*day)


# 2-day is probably a bit long for regular trips, but it fits more with the distribution
# Could be possible during incubation too I guess...
f3 <- trips$duration_s < 2*day
summary(f3)
f <- f & f3
# We've now excluded one-third of trips!
summary(f)

#  - I think the minimum time duration can be skipped if we use a distance threshold - very
# short distance trips are also likely to be of short time duration - so the two problems
# may then resolve together.
# # Minimum time duration
# hist(trips$duration_s[f])
# # place line every 5th hour
# hour <- 60 * 60
# abline(v = hour*seq(5,50,5),lty = 2, lwd = 2, col = "red")
# 
# 
# # View trips less than 5 hours
# hist(trips$duration_s[f & trips$duration_s < 5 * hour])
# # Lets reduce the bin width
# hist(trips$duration_s[f & trips$duration_s < 5 * hour],
#      breaks = 50)
# hist(trips$duration_s[f & trips$duration_s < 5 * hour],
#      breaks = 100)
# abline(v = hour*c(1:48),lty = 2, lwd = 2, col = "red")
# # There's not a really obvious cut-off. However there is
# # a drop in the number of trips per a time duration below
# # ca. 3500 seconds.
# abline(v = 3500, lty = 2, lwd = 2, col = "red")
# # What is this in hours?
# 3500 / hour
# # Just under an hour - that seems a reasonable time duration,
# # probably enough to travel a short distance forage a bit,
# # then return
# 
# # Minimum time duration filter
# f4 <- trips$duration_s > 3500
# summary(f4)
# f <- f & f4
# summary(f)


# *f5 - dist_max ----
# The maximum distance (km) reached from the nest
# It seems sensible to have a minimum distance threshold.
# If the bird only travels say 1 km, perhaps it is roosting
# on the beach further down the coast.
# Let's first have a look at what this looks like
hist(trips$dist_max[f])
hist(trips$dist_max[f], breaks = 20)
summary(trips$dist_max[f] > 300)
# Look at trips of less than 150 km
hist(trips$dist_max[f & trips$dist_max < 150], breaks = 20)

# Less than 20 km
hist(trips$dist_max[f & trips$dist_max < 20], breaks = 40)
# Here there is a fairly clear potential cut-off. There are
# quite a number of trips of 0-1 km and 1 - 2 km, but few
# 2 - 4 km, then a gradual increase.
# Let's then suggest a threshold of 3 km
abline(v = 3, lty = 2, lwd = 2, col = "red")
# This is the same threshold used in:
# Camphuysen, K.C.J., Shamoun-Baranes, J., Loon, E.E. van, Bouten, W., 2015. Sexually distinct foraging
# strategies in an omnivorous seabird. Mar Biol 162, 1417–1428. doi:10.1007/s00227-015-2678-9



win.metafile("trip_distance_filter.wmf", width = 14, height = 7)
par(mfrow=c(1,2))
hist(trips$dist_max[f], breaks = 20,
     ylab = "N trips", xlab = "Max distance (km)",
     main = "All trips")
# Add lines for days
# abline(v = c(day_h, day_h *2, day_h * 3, day_h *4, day_h *5),
#        lty = 2, lwd = 2, col = "red")

hist(trips$dist_max[f & trips$dist_max < 20], breaks = 40,
     ylab = "N trips", xlab = "Max distance (km)",
     main = "<20 km")
# Add lines for days
# abline(v = c(day_h, day_h *2, day_h * 3, day_h *4, day_h *5),
       # lty = 2, lwd = 2, col = "dark grey")
abline(v = c(3),
       lty = 2, lwd = 2, col = "red")
dev.off()




f5 <- trips$dist_max > 3
summary(f5)

summary(trips$dist_max[f] > 3)

f <- f & f5
summary(f)


# *f6 - interval_max ----
# Maximum time interval between GPS locations
# We don't want to include trips with long data gaps.
# If there is a very long gap there could potentially
# be two trips included within one. Given that the 
# algorithm to dettect trips works simply on starting 
# a trip when the bird leaves the island to when it
# returns to the island.
hour <- 60*60
# First view this paramater
hist(trips$interval_max[f])
# Cut-down to less than 10 hours (using the hour that we
# defined earlier - the number of seconds in an hour)
hist(trips$interval_max[f & trips$interval_max < 10 * hour])
# 5 hours
hist(trips$interval_max[f & trips$interval_max < 5 * hour])

# 1 hour
hist(trips$interval_max[f & trips$interval_max < hour],
     breaks = 20)
# Indicated half an hour (red)
abline(v = 0.5 * hour, lty = 2, lwd = 2, col = "red")
# 15 minutes (blue)
abline(v = 0.25 * hour, lty = 2, lwd = 2, col = "blue")

# I think for this analysis 30 minutes should be adequate,
# for most trips this shouldn't miss too much.
f6 <- trips$interval_max < 0.5 * hour
summary(f6)



win.metafile("trip_time_interval_filter.wmf", width = 14, height = 7)
par(mfrow=c(1,2))
hist(trips$interval_max[f & trips$interval_max < 24*hour]/60, breaks = 40,
     ylab = "N trips", xlab = "Maximum time interval (mins)",
     main = "All trips")


hist(trips$interval_max[f & trips$interval_max < hour]/60, breaks = 40,
     ylab = "N trips", xlab = "Maximum time interval (mins)",
     main = "<1 h")
# Add lines for days
# abline(v = c(day_h, day_h *2, day_h * 3, day_h *4, day_h *5),
# lty = 2, lwd = 2, col = "dark grey")
abline(v = c(30),
       lty = 2, lwd = 2, col = "red")
dev.off()




f <- f & f6
summary(f)


# # *f7 - date-period -----
# # Months are stored as text, as a factor. They are numbered.
# # 01 - January
# # 02 - February, etc. ...
# 
# # Filter to include only trips during May (05), June (06), and July (07)
# f7 <- trips$month == "05" |  trips$month == "06" |  trips$month == "07"
# 
# summary(f7)
# # Add to existing filter
# f <- f & f7
# # See how many trips we now have (TRUE)
# summary(f)

# *Period of interest------

# Filter to include only trips from 20th May to 21st July

f7 <- (trips$start_time>="2011-05-20" & trips$start_time<="2011-07-21")| +
  (trips$start_time>="2012-05-20" & trips$start_time<="2012-07-21")| +
  (trips$start_time>="2013-05-20" & trips$start_time<="2013-07-21")

# f7 <- trips$start_time >= "05" &  trips$start_time == "06"

summary(f7)
# Add to existing filter
f <- f & f7
# See how many trips we now have (TRUE)
summary(f)



# Summary ----
# We applied the following 7 filters:
# Non-migration trips
f0 <- trips$trip_type == 0

# At least 10 GPS locations
f1 <- trips$fix_n >= 10

# Note there is no f2!

# Trips of less than 2 days
day <- 24*60*60
f3 <- trips$duration_s < 2*day

# Note there is no f4!

# Trips where the furthest point reached is at least 3 km
f5 <- trips$dist_max > 3

# Trips with no more than 30 minutes between GPS locations
hour <- 60*60
f6 <- trips$interval_max < 0.5 * hour

# Trips from 20th May to 21st July only (for the 3 years of the study)
f7 <- (trips$start_time>="2011-05-20" & trips$start_time<="2011-07-21")| +
  (trips$start_time>="2012-05-20" & trips$start_time<="2012-07-21")| +
  (trips$start_time>="2013-05-20" & trips$start_time<="2013-07-21")

# Trips from 20th May to 21st July only (for the 3 years of the study)
# f7 <- (trips$start_time>="2011-04-30" & trips$start_time<="2011-07-21")| +
#   (trips$start_time>="2012-04-30" & trips$start_time<="2012-07-21")| +
#   (trips$start_time>="2013-04-30" & trips$start_time<="2013-07-21")
# 


# Combine these filters
f <- f0 & f1 & f3 & f5 & f6 & f7

length(trips$fix_n)
# We started with 3321 trips ...
summary(f)
# ... and ended up with 1038 trips that met the above criteria

# Sample sizes ----
# First install package 'reshape2' which makes this
# quite easy to look at
# install.packages("reshape2")
library(reshape2)

# Number of trips per individual
aggregate(fix_n ~ ring_number,
          data = trips,
          FUN = length)

# Number of trips per year
aggregate(fix_n ~ year,
          data = trips,
          FUN = length)

# Number of trips per month
aggregate(fix_n ~ month,
          data = trips,
          FUN = length)

# Number of trips per individual by year
aggregate(fix_n ~ year + ring_number ,
          data = trips,
          FUN = length)

# Number of trips per individual by month and year
aggregate(fix_n ~ month + year + ring_number ,
          data = trips,
          FUN = length)

# Number of individuals
length(unique(trips$ring_number))

# Output filtered data ----
# For later analysis we can save the filtered data

# Output to an R binnary file
# Filter the trips data frame
trips.f <- trips[f,]

# Save as an R data file
save(trips.f, file = "foraging_trip_info_filtered_jan2016.RData")
# save(trips.f, file = "foraging_trip_info_filtered_jan2016_ext.RData")

# Output to a csv file
write.csv(trips.f, file = "foraging_trip_info_filtered_jan2016.csv")

# # Output to an Excel file
# # First install required library
# install.packages("xlsx")
# install.packages("rJava")
