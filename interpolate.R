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
