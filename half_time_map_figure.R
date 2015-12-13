
#If needed install:
install.packages("maptools")


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
png("range_map_halftime.png", width = 3*dpi, height = 5.5*dpi, res = dpi)
# win.metafile("range_map.wmf", width = 4, height = 5)
# ?pdf
# Set map limits and format
par(mfrow=c(1,1))
par( mar = c(5, 4, 4, 2))
c.xlim <- c(5,30)
c.ylim = c(50,70)

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

rect(17.0, 56.9, 19, 57.6, lwd = 2, lty = 1)

# Scale bar and axis
x <- c.xlim[1] + (c.xlim[2] - c.xlim[1])/20
y <- c.ylim[1] + (c.ylim[2] - c.ylim[1])/10
map.scale(x, y, ratio = FALSE, relwidth = 0.50, cex = 1)
#   ?map.scale
box()
axis(side=(1),las=1, cex.axis = 0.6)
axis(side=(2),las=1, cex.axis = 0.6)

dev.off()


par(mfrow=c(1,2))

# Large area  map
# Plot map and output to file
png("zoom_map_gotland_halftime.png", width = 3*dpi, height = 5*dpi, res = dpi)
# win.metafile("zoom_map.wmf", width = 5, height = 5)

# ?png
# Set map limits and format
par(mfrow=c(1,1))
par( mar = c(5, 4, 4, 2))


c.xlim <- c(17.9, 18.6)
c.ylim = c(56.9,57.5)

# data(worldHiresMapEnv)

plot(gadm, xlim = c.xlim,
     ylim = c.ylim, col="dark grey", bg = "white")


# Add rectangle to indicate region in expansion map
rect(18.27, 57.21, 18.39, 57.247, col = "black", lwd = 1, lty = 1)

# Scale bar and axis
# x <- c.xlim[1] + (c.xlim[2] - c.xlim[1])/20
# y <- c.ylim[1] + 9*(c.ylim[2] - c.ylim[1])/10
# map.scale(x, y, ratio = FALSE, relwidth = 0.2, cex = 0.8)
x <- c.xlim[1] + (c.xlim[2] - c.xlim[1])/20
y <- c.ylim[1] + (c.ylim[2] - c.ylim[1])/10
map.scale(x, y, ratio = FALSE, relwidth = 0.50, cex = 1)
box()
axis(side=(1),las=1, cex.axis = 0.6)
axis(side=(2),las=1, cex.axis = 0.6)

text(x= c(17.95), y = c(57.25), labels = c("SK"), adj = NULL,
     pos = NULL, offset = 0, vfont = NULL,
     cex = 1, col = NULL, font = 3)

text(x= c(18.4), y = c(57.45), labels = c("Gotland"), adj = NULL,
     pos = NULL, offset = 0, vfont = NULL,
     cex = 1, col = NULL, font = 3)

# text(x= c(17.97), y = c(57.285), labels = c("*"), adj = NULL,
#      pos = NULL, offset = 0, vfont = NULL,
#      cex = 2, col = NULL, font = 1)

dev.off()








# Large area  map
# Plot map and output to file
png("zoom_map_karlso_halftime.png", width = 7*dpi, height = 5.5*dpi, res = dpi)
# win.metafile("zoom_map.wmf", width = 5, height = 5)

# ?png
# Set map limits and format
par(mfrow=c(1,1))
# par( mar = c(5, 4, 4, 2))
par( mar = c(5, 4, 4, 2))


c.xlim <- c(16, 19)
c.ylim = c(56.75,57.65)

# data(worldHiresMapEnv)

plot(gadm, xlim = c.xlim,
     ylim = c.ylim, col="dark grey", bg = "white")


# Add rectangle to indicate region in expansion map
rect(18.27, 57.21, 18.39, 57.247, col = "black", lwd = 1, lty = 1)

# Scale bar and axis
# x <- c.xlim[1] + (c.xlim[2] - c.xlim[1])/20
# y <- c.ylim[1] + 9*(c.ylim[2] - c.ylim[1])/10
# map.scale(x, y, ratio = FALSE, relwidth = 0.2, cex = 0.8)
x <- c.xlim[1] + (c.xlim[2] - c.xlim[1])/3
y <- c.ylim[1] + (c.ylim[2] - c.ylim[1])/10
map.scale(x, y, ratio = FALSE, relwidth = 0.30, cex = 1)
box()
axis(side=(1),las=1, cex.axis = 0.6)
axis(side=(2),las=1, cex.axis = 0.6)

text(x= c(17.95), y = c(57.25), labels = c("SK"), adj = NULL,
     pos = NULL, offset = 0, vfont = NULL,
     cex = 1, col = NULL, font = 3)

text(x= c(18.4), y = c(57.45), labels = c("Gotland"), adj = NULL,
     pos = NULL, offset = 0, vfont = NULL,
     cex = 1, col = NULL, font = 3)
# ?text
text(x= c(16.65), y = c(56.80), labels = c("Öland"), adj = NULL,
     pos = NULL, offset = 0, vfont = NULL,
     cex = 1, col = NULL, font = 3, srt = 70)

text(x= c(16.2), y = c(57.4), labels = expression("Sweden\n mainland"), adj = NULL,
     pos = NULL, offset = 0, vfont = NULL,
     cex = 1, col = NULL, font = 3)

# text(x= c(17.97), y = c(57.285), labels = c("*"), adj = NULL,
#      pos = NULL, offset = 0, vfont = NULL,
#      cex = 2, col = NULL, font = 1)

dev.off()

