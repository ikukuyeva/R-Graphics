#_____________________________________
# Author: Irina Kukuyeva
# Course: Intermediate Graphics with R
# Date:   November 16, 2009
#_____________________________________

# Solution to Exercise I

beanplot(Sepal.Width ~ Species, data=iris, col=list(grey(0.5),c(grey(0.8),"white")), border = NA, overallline = "median", ll=0.01, side="both")

# Solution to Exercise II
source("http://www.biostat.jhsph.edu/~rpeng/RR/mvtsplot/mvtsplot.R")		
mvtsplot(EuStockMarkets)

# Solution to Exercise III
ozone<-read.table("http://www.ats.ucla.edu/stat/R/faq/ozone.csv", sep=",", header=T)
attach(ozone)
plot(Lat~Lon, xlim=c(-125, -113), ylim=c(31,42))
map("state", "california", add=TRUE)

# Solution to Exercise IV
ozone<-read.table("http://www.ats.ucla.edu/stat/R/faq/ozone.csv", sep=",", header=T)
attach(ozone)
library(rgl)
plot3d(z=Av8top, x=long, y=lat)

# Solution to Exercise V
# Sample from the random uniform:
x <- sort(runif(100, min=0, max=10))
y <- x+runif(1)
f <- function(x,y) { r <- sin(1.5*x)/y}
z <- outer(x,y,f)
persp(x, y, z, col = terrain.colors(length(z)/4), shade = 0.1, ticktype = "detailed", expand=0.7)

