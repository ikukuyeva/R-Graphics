# 08/17/09
# Graphics Mini-Course R Code

#____________________________________________
### Summary Plots
#____________________________________________
survey = read.table("http://www.stat.ucla.edu/~mine/students_survey_2008.txt", header = TRUE, sep = "\t")
names(survey)
attach(survey)

# Segmented Bar Charts
	barplot(table(gender, hand), col = c("skyblue", "blue"), main = "Segmented Bar Plot \n of Gender")
	legend("topleft", c("females","males"), col = c("skyblue", "blue"), pch = 16, inset= 0.05)

# Pie Chart
	pie(table(gender))

# Histograms
	# 1.
		hist(ageinmonths, main="Histogram of Age (Mo)")
		abline(v=mean(ageinmonths), col = "blue")
		abline(v=median(ageinmonths), col = "green")
		legend("topright", c("Mean", "Median"), pch = 16, col = c("blue", "green"))

	# 2.
		data(presidents)		
		hist(presidents, prob=T, ylim=c(0, 0.04), breaks=20)
		lines(density(presidents, na.rm=TRUE), col="red")
		mu<-mean(presidents, na.rm=TRUE) 
		sigma<-sd(presidents, na.rm=TRUE)
		x<-seq(10,100,length=100) 
		y<-dnorm(x,mu,sigma) 
		lines(x,y,lwd=2,col="blue") 

# Box-Whisker Plot
	data(quakes)		
	# Subset the magnitude:
	ind<-ifelse(quakes[, 4]<4.5, 0, 1)
	ind<-as.factor(ind)
	boxplot(quakes[, 4]~ind)
	# Alternatively:
	library(lattice)
	bwplot(quakes[, 4]~ind, xlab=c("Mag<4.5", "Mag>=4.5"), ylab="Magnitude")

# Beanplot
	library(beanplot)		
	par(mfrow=c(1,2))
	data(airquality)
	boxplot(airquality[, 2], main="Boxplot", xlab="Solar")
	beanplot(airquality[, 2], main="Beanplot", xlab="Solar")
	par(mfrow=c(1,1))

# Scatterplot
	data(quakes)
	library(car)		
	scatterplot.matrix(quakes[, 1:4])

# Equality of Distributions
	library(lattice)
	survey = read.table("http://www.stat.ucla.edu/~mine/students_survey_2008.txt", header = TRUE, sep = "\t")
	attach(survey) 
	qq(gender~ageinmonths)

# Identify-ing Obs
	# Generate data and fit a regression curve:
	set.seed(3012008)
	x=rnorm(100); y=-x+I(x^2) +rnorm(100)
	fit<-lm(y~x+I(x^2)); fit
		
	# Plot the resulting regression curve:
	plot(y~x, pch=19)
	curve(expr=fit[[1]][1]+fit[[1]][2]*x+fit[[1]][3]*I(x^2), from=range(x)[1], to=range(x)[2], add=TRUE, col="blue", lwd=2)

	# Identify the "outlying" observations:
	# index<-identify(y~x); index	

#____________________________________________
### TS Plots
#____________________________________________
# Univariate
	data(EuStockMarkets)
	dax<-EuStockMarkets[, 1]
	plot(dax)

# Multivariate
	# 1.
		# Convert data to a time series via ts() or zoo():
		data(airquality)
		a<-airquality[, 1:3]
		time<-ts(1:nrow(a), start=c(1973, 5), frequency=365)
		# If your data is stored as a data frame,
		# coerce it to be a matrix via as.matrix()
		class(a)
		a.mat<-as.matrix(a)

		# Make a time series of the two (or more variables)
		library(zoo)
		name.zoo<-zoo(cbind(a.mat[, 1], a.mat[, 2]))
		colnames(name.zoo)<-c("Ozone", "Solar")
		#### Plot the variables
		plot(name.zoo)

	#2.
		# Load MTVSPLOT function code
		# After processing data as in Approach 1
		# Plot the variables
		 mvtsplot(name.zoo)
		# Purple=low, grey=medium, green=high, white=missing values

	#3.
		# After processing data as in Approach 1
		# load both libraries:
		library(lattice)
		library(zoo)
		data(EuStockMarkets)
		z<-EuStockMarkets
		xyplot(z, screen = c(1,1,1,1), col = 1:4, strip = FALSE)
		legend(1992, 5000, colnames(z), lty = 1, col = 1:4)

#____________________________________________
### Geo-Plots
#____________________________________________
# 1.
	data(quakes)
	library(maps)
	plot(quakes[, 2], quakes[, 1], xlim=c(100, 190), ylim=c(-40, 0))
	map("world", add=T)

# 2.
	library(mapproj)
	library(maps)
	m <- map('world',plot=FALSE)
	# Projection is Azimuthal with equal-area
	map('world',proj='azequalarea',orient=c(longitude=0,latitude=180,rotation=0))
	map.grid(m,col=2)
	points(mapproject(list(y=quakes[which(quakes[, 4]>=6), 1],x=quakes[which(quakes[, 4]>=6), 2])),col="blue",pch="x",cex=2)

# Bonus:
	# Number of points in ocean after filtering:
	in.what.country<-map.where(database="world", quakes[, 2], quakes[, 1])
	ind<-sum(is.na(in.what.country)); ind
	# Number of observations: 1000
	# Number in Ocean: 993


#____________________________________________
### 3D Plots
#____________________________________________
# lattice
	# 1.
		library(lattice)
		wireframe(volcano, color.palette = terrain.colors, asp = 1, color.key=TRUE, drape=TRUE, scales = list(arrows = FALSE))
	# 2.
		library(lattice)
		levelplot(volcano, color.palette = terrain.colors, asp = 1, color.key=TRUE, drape=TRUE, scales = list(arrows = FALSE), add=TRUE)
		contour(volcano, add=TRUE, lwd=1.3, labcex=1.3)

# rgl
	library(rgl)
	data(quakes)	
	plot3d(x=quakes[, 2], y=quakes[, 1], z=quakes[, 3], xlab="Longitude", ylab="Latitude", zlab="Depth")

#____________________________________________
### Simulations
#____________________________________________
x=5:6; y=1:3
outer(x,y)

fcn<-function(x,y){z=x+y}
outer(x,y,fcn)

# Sample from the random uniform:
x <- sort(runif(100, min=0, max=10))
y <- x+runif(1)
f <- function(x,y) { r <- y*sin(x)}
z <- outer(x,y,f)
persp(x, y, z, col = "lightblue", shade = 0.1, ticktype = "detailed", expand=0.7)

contour(x,y,z)



