#11/01/09
# ggplot2:
data(quakes)
attach(names)
library(ggplot2)

# recoding mag:
index1<-rep(NA, nrow(quakes))
index2<-rep(NA, nrow(quakes))
index3<-rep(NA, nrow(quakes))
index1<-ifelse(mag<5, 1, 0)
index2<-ifelse(mag >=5 & mag<6, 2, 0)
index3<-ifelse(mag >=6, 3, 0)

index<-rep(NA, nrow(quakes))
for(i in 1:nrow(quakes)){
	index[i]<-max(index1[i], index2[i], index3[i])
	}
qplot(long, lat, depth, data=quakes, size=index, colour=factor(index))->p

p+panels(xlabel="Longitude", ylabel="Latitude")
# add labels to the index:
p + scale_size(labels=c("<5", "b/w 5 and 6", ">6"), breaks=c(1,2,3), name="Magnitude")


outlines<-map("world", xlim=c(100, 190), ylim=c(-40,0))
map_coords<-as.data.frame(c(outlines$x, outlines$y))
map <- c( geom_path(aes(x = x, y = y), data = map_coords)


ggsave("plot.png", width = 5, height = 5)





library(maps)
reg <- as.data.frame(map("world", xlim = c(100, 190), ylim = c(-40, 0), plot = FALSE)[c("x", "y")])
qplot(x, y, data=reg, type="path")

ggpath(p, data=reg)



, colour = alpha("grey20", 0.2)), 
  scale_x_continuous("", limits = c(-113.8, -56.2), breaks = c(-110, -85, -60)), 
  scale_y_continuous("", limits = c(-21.2, 36.2))