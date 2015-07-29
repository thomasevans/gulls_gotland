

trips <- read.csv("foraging_trip_info_filtered_july2015.csv",
                  header = TRUE)









str(trips)







# Check variables - types (factor etc), distributions etc.
trips$ring_number <- as.factor(trips$ring_number)
trips$trip_id <- as.factor(trips$trip_id)

trips$start_time <- as.POSIXct(trips$start_time, tz = "utc")


hist(trips$gotland_time_prop, breaks = 40)
abline(h = 50)

plot(trips$gotland_time_prop ~ trips$gotland)

gotland_on <- rep(TRUE,length(trips$gotland_time_prop))
gotland_on[trips$gotland_time_prop < 0.2] <- FALSE




summary(gotland_on)


# Plot trip type by date -----
# Suggested by Gabriel to get some visualisation of the data
y <- (trips$year == 2012) & trips$ring_number == 8111250
plot(gotland_on[y]~trips$start_time[y],
     col = as.numeric(trips$ring_number),
     pch = 16)

summary(as.factor(trips$gotland))



# Modelling of data....

library("lme4")


install.packages("arm")

library("arm")

summary(is.na(trips))

?MuMIn::dredge
?options
11!
factorial(11)

2^11
?AIC

c()


x <- c(1,2)
y <- c(1.56, 4.5)

plot(y~x, ylim = c(0,8))
se <- c(0.13,.03)
?lines





# Model averaging stuff ------
# Should look at following function to get unconditional SE
modavgpred {AICcmodavg}
