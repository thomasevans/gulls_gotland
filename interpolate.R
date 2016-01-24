install.packages("signal")
library("signal")

xf <- seq(0, 11, length=50)
yf <- sin(2*pi*xf/5)
xp <- c(0:10)
yp <- sin(2*pi*xp/5)
pch  <- pchip(xp, yp, xf)
plot(xp, yp, xlim = c(0, 11),pch =10)
points(xf, pch, col = "orange")

?pchip

?interp1



# Try adehabitat packages
install.packages("adehabitatLT")
library("adehabitatLT")

# Rediscrete in time - linear interpolation
?redisltraj

# Example

data(puechcirc)

puechcirc

## before rediscretization
plot(puechcirc, perani = FALSE)

## after rediscretization
toto <- redisltraj(puechcirc, 400, nnew = 5)
plot(toto, perani = FALSE)


# Could use this method, though looks like it's most suited to beggining with a fairly regular time interval
install.packages("zoo")

library("zoo")

?na.spline
?na.approx