

# Required package
library("maps")
library("mapdata")
library("maptools")


# Set resolution for png device
dpi <- 300

# High res map data
load("SWE_adm0.RData")


# Read in distribution polygons and get into require format
kml_coord <- getKMLcoordinates(kmlfile="L. f. fuscus range.kml", ignoreAltitude=T)
head(kml_coord[[1]])
poly_1 <- Polygon(kml_coord[[1]])
p1 <- Polygons(list(poly_1), ID = "drivetime")
poly_2 <- Polygon(kml_coord[[2]])
p2 <- Polygons(list(poly_2), ID = "drivetime")
poly_3 <- Polygon(kml_coord[[3]])
p3 <- Polygons(list(poly_3), ID = "drivetime")

p1_sp <- SpatialPolygons(list(p1),proj4string=CRS("+init=epsg:4326"))
p2_sp <- SpatialPolygons(list(p2),proj4string=CRS("+init=epsg:4326"))
p3_sp <- SpatialPolygons(list(p3),proj4string=CRS("+init=epsg:4326"))


# Large area  map
# Plot map and output to file
# png("test_map_1.png", width = 4*dpi, height = 5*dpi, res = dpi)
win.metafile("test_map_1.wmf", width = 4, height = 5)
# ?pdf
# Set map limits and format
par(mfrow=c(1,1))
par( mar = c(5, 4, 4, 2))
c.xlim <- c(0,35)
c.ylim = c(52,72)

# c.xlim <- c(17,20)
# c.ylim = c(56.5,58)

data(worldHiresMapEnv)

map('worldHires', xlim = c.xlim,
    ylim = c.ylim, col="dark grey", density = 25, bg = "white",
    fill = TRUE)

# Add distribution polygons
col.grey <- rgb(0.2,0.2,0.2, 0.5)

plot(p1_sp , col = "dark grey", border = NA, add = T)
plot(p2_sp , col = "dark grey", border = NA, add = T)
plot(p3_sp , col = "dark grey", border = NA, add = T)

map('worldHires', col="black", density = 10, bg = NA, add = TRUE)

rect(17, 56.5, 20, 58, lwd = 1, lty = 1)

# Scale bar and axis
x <- c.xlim[1] + (c.xlim[2] - c.xlim[1])/20
y <- c.ylim[1] + (c.ylim[2] - c.ylim[1])/10
map.scale(x, y, ratio = FALSE, relwidth = 0.25)
#   ?map.scale
box()
axis(side=(1),las=1, cex.axis = 0.8)
axis(side=(2),las=1, cex.axis = 0.8)

dev.off()




# Large area  map
# Plot map and output to file
# png("test_map_2.png", width = 4*dpi, height = 5*dpi, res = dpi)
win.metafile("test_map_2.wmf", width = 4, height = 5)

# ?png
# Set map limits and format
par(mfrow=c(1,1))
par( mar = c(5, 4, 4, 2))


c.xlim <- c(17,19)
c.ylim = c(56.5,58)

# data(worldHiresMapEnv)

plot(gadm, xlim = c.xlim,
    ylim = c.ylim, col="dark grey", bg = "white")


# Add rectangle to indicate region in expansion map
rect(17.9, 57.18, 18.4, 57.33, lwd = 1, lty = 1)

# Scale bar and axis
x <- c.xlim[1] + (c.xlim[2] - c.xlim[1])/20
y <- c.ylim[1] + (c.ylim[2] - c.ylim[1])/10
map.scale(x, y, ratio = FALSE, relwidth = 0.25)
box()
axis(side=(1),las=1, cex.axis = 0.8)
axis(side=(2),las=1, cex.axis = 0.8)

dev.off()
