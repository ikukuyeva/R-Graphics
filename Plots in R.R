# 11/03/09
# Per David's email of Masanao's code:

# Code creates points in colors that faint out:

library(plotrix)
library(RColorBrewer)
plot(c(0,1),c(1,0))
x<-seq(0,1,0.1)
y<-x
co=brewer.pal(11,"RdBu")
for (i in 1:length(x)){
   draw.circle(x[i],y[i],r=0.01,border=1,col=co[i],lwd=0.3)
}

for (i in 1:length(x)){
   draw.circle(x[i],y[12-i],r=0.01,border=1,col=rgb(x[i],1,1,0.4),lwd=0.3)
}

xx<-runif(1000,0,1)
yy<-runif(1000,0,1)
plot(c(0,1),c(1,0))
for (i in 1:length(xx)){
   draw.circle(xx[i],yy[i],r=0.01,border=1,col=rgb(xx[i],1,yy[i],0.4),lwd=0.3)
}