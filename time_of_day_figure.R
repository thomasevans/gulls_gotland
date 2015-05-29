# Time of day figure


# Read in data ----
trips <- read.csv("foraging_trip_info_filtered_may2015.csv", header = TRUE)
str(trips)


# Filter out two birds whose data are not included in the statistical model (too few observations)
f <- (trips$ring_number != 8114317) & (trips$ring_number != 8114320)
summary(f)
trips_f <- trips[f,]

# Prepare data for histogram -----
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


# Trip to Gotland?
gotland_on <- rep(FALSE,length(trips_f$gotland_time_prop))
gotland_on[trips_f$gotland_time_prop > 0.2] <- TRUE
trips_f$gotland_on <- gotland_on

# summary(g)
g  <- trips_f$gotland_on == TRUE
x2 <- sapply(trips_f$time_since_sunrise_h[g], range.fun)
x3 <- sapply(trips_f$time_since_sunrise_h[g],range.fun.2)
gotland_trips <- (cbind(x2,x3))

x4 <- sapply(trips_f$time_since_sunrise_h[!g], range.fun)
x5 <- sapply(trips_f$time_since_sunrise_h[!g],range.fun.2)
non_gotland_trips <- (cbind(x4,x5))

x6 <- sapply(trips_f$time_since_sunrise_h, range.fun)
x7 <- sapply(trips_f$time_since_sunrise_h,range.fun.2)
all_trips <- (cbind(x6,x7))



# Plot histogram -----
# See: http://stackoverflow.com/questions/3541713/how-to-plot-two-histograms-together-in-r
# There there are some good answers for how to display two sets of data on the same histogram

# library(scales)

# Plot Gotland trips first
# hist(gotland_trips, xlim = c(-24,24), ylim = c(0,100), breaks = 48, col='skyblue',
#      border = TRUE, main = "", xlab = "Time sinse sunrise (h)",
#      ylab = "N foraging trips per hour")

win.metafile(filename = "time_of_day.wmf", width = 7, height = 7, pointsize = 12)


# Plot all trips first
hist(all_trips, xlim = c(-4,20), ylim = c(0,120), breaks = 48, col='white',
     border = TRUE, main = "", xlab = "Time sinse sunrise (h)",
     ylab = "N  foraging trips per hour",
     xaxt = "n",
     las = 1,
     cex.lab = 1.2,
     cex.axt = 1.2,
     mgp=c(3,1,0))

axis(1, at = seq(-4,20,2), labels = seq(-4,20,2), pos = 0,
     cex = 1.2)
# ?axis
# Of which Gotland trips
hist(gotland_trips, xlim = c(-4,20), ylim = c(0,100), breaks = 48, col='grey',
     border = TRUE, add = TRUE)


# points()

# Add non-Gotland trips
# hist(non_gotland_trips, add = TRUE, col = scales::alpha('red',.5),
#      border = TRUE,  xlim = c(-24,24), breaks = 48)

# Time of night
# abline(v = -6.565155023, lwd = 2, lty = 2)

night.mean <- -6.565155023
night.max <- -8.43
night.min <- -6.03

rect(-23.9, 115, 24, 125, density = NULL, angle = 45,
     col = "grey 95", border = FALSE, lty = par("lty"), lwd = par("lwd"))
rect(0, 115, night.mean, 125, density = NULL, angle = 45,
     col = "black", border = FALSE, lty = par("lty"), lwd = par("lwd"))
rect(24, 115, 24 + night.max, 125, density = NULL, angle = 45,
     col = "grey 40", border = FALSE, lty = par("lty"), lwd = par("lwd"))
rect(24, 115, 24 + night.mean, 125, density = NULL, angle = 45,
     col = "grey 20", border = FALSE, lty = par("lty"), lwd = par("lwd"))
rect(24, 115, 24 + night.min, 125, density = NULL, angle = 45,
     col = "black", border = FALSE, lty = par("lty"), lwd = par("lwd"))
rect(-23.9, 115, 24, 125, density = NULL, angle = 45,
     col = NA, border = TRUE, lty = par("lty"), lwd = par("lwd"))



# Percentages
time_sunrise <- sapply(trips_f$time_since_sunrise_h, range.fun.2)

# cbind(time_sunrise,trips_f$time_since_sunrise_h)


hours <- 0:24
x <- floor(time_sunrise)

n_all <- table(x)
n_got <- table(x[g])
p_got <- as.vector(n_got)/as.vector(n_all)
p_got_perc <- 100*(p_got)

# plot(p_got_perc)
h1 <- seq(0.5,19.5,1)
h2 <- seq(-3.5,-0.5,1)
h <- c(h1,h2)

h2 <- c(h[21:24], h[1:20])
p_got_perc2 <- c(p_got_perc[21:24], p_got_perc[1:20])
points(p_got_perc2~h2)
lines(p_got_perc2~h2, lwd = 1.5, lty = 2)


# Make it a bit prettier
# box()
dev.off()