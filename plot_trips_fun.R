# A file to source to plot maps of foraging trips


plot.trips <- function(long = NA, lat = NA, trip_ids = NA,
                       alpha = 0.7, brewer.pal.name = "Paired",
                       for_points_sea = NA,
                       for_points_got = NA,
                       other_points = NA,
                       gotland_poly = FALSE){
  
  trip_ids_unique <- unique(trip_ids)
  trip.n <- length(trip_ids_unique)
  
  # Function to plot a base map ----
  # Plot a map with coastlines only - no 
  require(maps)
  require(RColorBrewer)
  require(RColorBrewer)
  require(sp)
  
  # Coastline data
  load("SWE_adm0.RData")
  
  map.base.fun <- function(xlim = c(17,18.3), ylim =  c(57,57.7)){
    par(mfrow=c(1,1))
    par( mar = c(5, 4, 4, 2))
    #   par(bg = 'white')
    
    # col.green <- brewer.pal(5,"Greens")
    # col.blue <- brewer.pal(5,"Blues")
    
    
    plot(gadm, xlim = xlim,
         ylim = ylim, col= "grey40", bg = "white",
         lty = 0)
    
    ## Scale bar and axis
    box(col="black",lwd=2)
    axis(side=(1),las=1,col="black",col.axis="black")
    axis(side=(2),las=1,col="black",col.axis="black")
    
  }
  
  # Alpha channel ----
  addalpha <- function(colors, alpha=1.0) {
    r <- col2rgb(colors, alpha=T)
    # Apply alpha
    r[4,] <- alpha*255
    r <- r/255.0
    return(rgb(r[1,], r[2,], r[3,], r[4,]))
  }
  
  # hack map.scale function
  # map.scale2 <- map.scale
  # fix(map.scale2)
  # source("D:/Dropbox/R_projects/murre_gps/map.scale2.R")
  
  # Plot trips ----
  
#   # Suppress warnings from brewer.pal
  oldw <- getOption("warn")
  options(warn = -1)
  
  # lbbg_flight_ids <- unique(gps_lbbg$flight_id)
  # col.vec <- rainbow(length(land_10))
  col.vec <- brewer.pal(trip.n, brewer.pal.name)
  col.vec.al <- addalpha(col.vec, alpha = alpha)
  col.vec.al.rand <- col.vec.al[sample(seq_along(col.vec.al))]
  
  if(length(col.vec.al.rand) < trip.n){
    col.vec.al.rand <- c(col.vec.al.rand,col.vec.al.rand)
  }
  
  
  options(warn = oldw)
  
  # Plot all trips -------
  
  
  xlim.n <- range(long)
  ylim.n <- range(lat)
  
  x.range <- xlim.n[2] - xlim.n[1]
  
  xlim.n <- c((xlim.n[1]-0.1*x.range), (xlim.n[2]+0.1*x.range))
  
  y.range <- ylim.n[2] - ylim.n[1]
  
  ylim.n <- c((ylim.n[1]-0.1*y.range), (ylim.n[2]+0.1*y.range))
  
  

  
  map.base.fun(xlim = xlim.n, ylim = ylim.n)
  
  
  if(gotland_poly){
    
    gotland_long   <-  c(18.69902, 18.63378, 18.61279, 18.56489, 18.49318, 18.44077, 18.30666, 18.2061, 18.09348, 18.10016, 18.17383, 18.15944, 18.08482, 18.09232, 18.13166, 18.15832, 18.13748, 18.16773, 18.20654, 18.19873, 18.23659, 18.28966, 18.28202, 18.23032, 18.19989, 18.17564, 18.12695, 18.19584, 18.31607, 18.38137, 18.41146, 18.37254, 18.35202, 18.42005, 18.4743, 18.53251, 18.53104, 18.51167, 18.40778, 18.69513, 18.78849, 18.69069, 18.8719, 18.94208, 19.02026, 18.9025, 18.80552, 18.83831, 18.82224, 19.04768, 19.19782, 19.39945, 19.32853, 19.2397, 19.1699, 19.02679, 18.84733, 18.81656, 18.77957, 18.69902)
    
    gotland_lat    <-  c(57.94159, 57.89255, 57.85448, 57.84177, 57.83054, 57.82949, 57.65937, 57.61784, 57.54344, 57.4375, 57.38398, 57.318, 57.28085, 57.26383, 57.23233, 57.18662, 57.16356, 57.13989, 57.12689, 57.05876, 57.06099, 57.09378, 57.04784, 57.03813, 57.02055, 56.98026, 56.91038, 56.89589, 56.92512, 56.97002, 56.98702, 57.01864, 57.05858, 57.09776, 57.11539, 57.11611, 57.1273, 57.13386, 57.12617, 57.22258, 57.26402, 57.29327, 57.38018, 57.40429, 57.43351, 57.45097, 57.45049, 57.60669, 57.66888, 57.73525, 57.88962, 57.94424, 57.97617, 57.99029, 57.99399, 57.93041, 57.92847, 57.87778, 57.92259, 57.94159)
    
    got_p <- Polygon(cbind.data.frame(gotland_long, gotland_lat))
    # ?sp::Polygon
    got_p <- Polygons(list(got_p), "s1")
    got_p <- SpatialPolygons(list(got_p))
    plot(got_p, col = NULL, border= addalpha("red", alpha = 0.5), lwd = 2, pbg=NULL, add = TRUE)
    # plot(got_p)
  }
  
  i <-13
  for(i in 1:trip.n){
    
    fx <- trip_ids == trip_ids_unique[i]
    n <- length(long[fx])
    # ?points
    # points(long[fx], lat[fx], col = col.vec.al.rand[i], cex = 0.5, lwd = 0.8)
    segments(long[fx][-1], lat[fx][-1],
             long[fx][1:n-1], lat[fx][1:n-1],
             col = col.vec.al.rand[i], lty = 1, lwd = 2)
    col.sym <- addalpha(c("dark blue","dark green",
                          "dark grey", "light grey"), alpha = 0.5)
    # col.sea <- addalpha("light blue", alpha = 0.8)
    points(long[other_points & fx], lat[other_points & fx],
           bg = col.sym[3], col = col.sym[4], cex = 0.6, pch = 21, lwd = 0.5)
    points(long[for_points_got & fx], lat[for_points_got & fx],
            bg = col.sym[2], col = col.sym[4], cex = 0.8, pch = 21, lwd = 0.5)
    points(long[for_points_sea & fx], lat[for_points_sea & fx],
           bg = col.sym[1], col = col.sym[4], cex = 0.8, pch = 21, lwd = 0.5)

  }
  map.scale(ratio = FALSE,
            relwidth = 0.30, cex = 1)
  
  
  
}
  
