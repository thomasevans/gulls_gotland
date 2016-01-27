# A file to source to plot maps of foraging trips



  
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
  
  
  # hack map.scale function
  # map.scale2 <- map.scale
  # fix(map.scale2)
  # source("D:/Dropbox/R_projects/murre_gps/map.scale2.R")
  
  # Plot trips ----
  
  # lbbg_flight_ids <- unique(gps_lbbg$flight_id)
  # col.vec <- rainbow(length(land_10))
  col.vec <- brewer.pal(10, "Paired")
  
  
  col.vec.al <- addalpha(col.vec, alpha = 0.7)
  col.vec.al.rand <- col.vec.al[sample(seq_along(col.vec.al))]
  
  
  # Plot all trips -------


  
  
  # i <- 12
  gps.s <- gps[gps$trip_id %in% land_10,]
  
  xlim.n <- range(gps.s$longitude)
  ylim.n <- range(gps.s$latitude)
  
  x.range <- xlim.n[2] - xlim.n[1]
  
  xlim.n <- c((xlim.n[1]-0.1*x.range), (xlim.n[2]+0.1*x.range))
  
  y.range <- ylim.n[2] - ylim.n[1]
  
  ylim.n <- c((ylim.n[1]-0.1*y.range), (ylim.n[2]+0.1*y.range))
  
  
  map.base.fun(xlim = xlim.n, ylim = ylim.n)
  i <- 1
  for(i in 1:length(land_10)){
    
    x <- land_10[i]
    # ?subset
    gps.sub <- gps.s[gps.s$trip_id == x,]
    n <- length(gps.sub$longitude)
    segments(gps.sub$longitude[-1], gps.sub$latitude[-1],
             gps.sub$longitude[1:n-1], gps.sub$latitude[1:n-1],
             col = col.vec.al.rand[i], lty = 1, lwd = 2)
  }
  map.scale(ratio = FALSE,
            relwidth = 0.25, cex = 1.2)