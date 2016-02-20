

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
# install.packages("BBMM")
library("BBMM")
unique(points$ring_number)

source("deg.dist.R")
points$coldist <- deg.dist( 17.971676, 57.278817,
                            points$longitude,
                            points$latitutde)
hist(points$coldist)

points.sub <- (points$dist_max < 200) & (points$ring_number == 8114315)
points.sub <- (points$dist_max < 200)
points.sub <- (points$dist_max < 200) & (points$coldist > 3)



# range(points$dist_max)

# ?brownian.bridge
BBMM <- brownian.bridge(x = points$longitude[points.sub],
                        y = points$latitutde[points.sub], 
                        time.lag = rep(5,nrow(points[points.sub,])-1),
                        location.error = 0.05, 
                        cell.size = 0.05)
bbmm.summary(BBMM)
cont.levels <- c(seq(25, 75, by = 25), 90, 95, 99)
contours <- bbmm.contour(BBMM, levels= cont.levels, plot = TRUE)
points(x = points$longitude[points.sub], y = points$latitutde[points.sub])

# contours <- bbmm.contour(BBMM, levels=c(95, 99),
                         # locations=cbind.data.frame(points$longitude,
                                                    # points$latitutde), plot=TRUE)

library(maptools)
library(raster)
out <- data.frame(x=BBMM$x,y=BBMM$y,z=BBMM$probability)
# Make sure the data is properly projected
out.raster <- rasterFromXYZ(out,crs=CRS("+proj=utm +zone=12 +datum=WGS84"),digits=2) 
raster.contour <- rasterToContour(out.raster,levels=contours$Z) 
raster.contour <- spChFIDs(raster.contour,paste(cont.levels,"% Contour Line",sep=""))


# Map the contours nicely -------
# Maps package
library(maps)

# Load coastline data
load("SWE_adm0.RData")


pdf("map_thing.pdf", width = 5, height = 8)
par(mfrow=c(1,1))
par( mar = c(5, 4, 4, 2))

plot(gadm, col= "grey40", bg = "white",
     lty = 0,
     xlim = c(17,19.2),
     ylim = c(55.7,58.5))

# Alpha channel ----
addalpha <- function(colors, alpha=1.0) {
  r <- col2rgb(colors, alpha=T)
  # Apply alpha
  r[4,] <- alpha*255
  r <- r/255.0
  return(rgb(r[1,], r[2,], r[3,], r[4,]))
}


points(x = points$longitude[points.sub], y = points$latitutde[points.sub],
       cex = 0.2, col = addalpha("black", alpha = 0.2),
       lwd = 0.4)

plot(gadm, col = NA, bg = NA, lwd = 1, add = TRUE,
     border = addalpha("blue", alpha = 0.5))

# lwd.lines <- c(1:length(cont.levels))
# lwd.lines <- max(lwd.lines)/c(1:length(cont.levels))
# lwd.lines <- (lwd.lines/max(lwd.lines))*2.5
plot(raster.contour, add = TRUE, col = addalpha("red", alpha = 0.6),
     lwd = seq(1, 3, 2/length(cont.levels)))
     # lty = c(1:length(cont.levels)))


## Scale bar and axis
box(col="black",lwd=2)
axis(side=(1),las=1,col="black",col.axis="black")
axis(side=(2),las=1,col="black",col.axis="black")

map.scale(ratio = FALSE,
          relwidth = 0.30, cex = 1)
dev.off()
