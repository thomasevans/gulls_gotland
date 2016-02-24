# A file to source to plot maps of foraging trips


plot.trips <- function(long = NA, lat = NA, trip_ids = NA,
                       alpha = 0.7, brewer.pal.name = "Paired",
                       for_points_sea = NA,
                       for_points_got = NA,
                       other_points = NA){
  
  trip_ids_unique <- unique(trip_ids)
  trip.n <- length(trip_ids_unique)
  
  # Function to plot a base map ----
  # Plot a map with coastlines only - no 
  require(maps)
  require(RColorBrewer)
  require(RColorBrewer)
  
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
  
