

# Load in the points data ----
load("foraging_trip_info_filtered_jan2016_points_300s_ext.RData")

# Load in trip details data -----
load("foraging_trip_info.RData")

# Merge these data ------
points <- merge(out.df, trips, "trip_id")

# See how this looks
str(points)


# KDE thing -------

# Load packaged needed for kernel density estimator calculation
library("adehabitatHR")
library("sp")

# Get in format needed for adehabitatHR
# xy <- t(rbind(coords[f,1],coords[f,2]))
xy <- cbind.data.frame(points$longitude, points$latitutde)
names(xy) <- c("x", "y")
coordinates(xy) <- c("x","y")

# Make kernel object
kud <- kernelUD(xy, h = 1)
kud.lst <- list()
h <- c(0.1,0.4,0.5,0.8,1,2,3,5,7,10)
for(i in 1:10){
  kud.lst[i] <- kernelUD(xy, h = h[i])
}
par(mfrow=c(4,4))
for(i in 1:10){
  image(kud.lst[[i]])
}

plot(kud.lst[])

  
  kud
summary(kud)
image(kud)
# Plot 50% kernel to see how this looks
plot(getverticeshr(kud, 10), lwd = 2)
points(xy)

?getverticeshr


# Alternative approach with BBMM ----
# See: Kranstauber, B., Kays, R., LaPoint, S.D., Wikelski, M., Safi, K., 2012. A dynamic Brownian bridge movement model to estimate utilization distributions for heterogeneous animal movement. Journal of Animal Ecology.

# Load package
install.packages("BBMM")
library("BBMM")


?brownian.bridge
BBMM <- brownian.bridge(x = points$longitude, y = points$latitutde, 
                        time.lag = rep(5,nrow(points)), location.error = 20, 
                        cell.size = 50)
contours <- bbmm.contour(BBMM, levels=c(95, 99),
                         locations=cbind.data.frame(points$longitude,
                                                    points$latitutde), plot=TRUE)
par(mfrow=c(1,1))
plot(BBMM)
summary(BBMM)
