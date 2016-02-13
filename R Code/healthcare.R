### Healthcare data analysis for tutorial

### Step 0: Export as CSV from https://chhs.data.ca.gov/Healthcare/Number-of-Selected-Inpatient-Medical-Procedures-Ca/mdt8-gwyw
data <- read.csv("../data/Number_of_Selected_Inpatient_Medical_Procedures__California_Hospitals__2005-2014.csv",
				 stringsAsFactors=FALSE
				 )

### Summary plots 
library(car)	
scatterplotMatrix(
	x = data[, c("Latitude", "Longitude", "Volume")], 
	smoother = FALSE, 
	reg.line = FALSE
	)

?scatterplotMatrix # for documentation
library(car)		
scatterplotMatrix(
	x = data[, c("Latitude", "Longitude", "Volume")], 
	reg.line = FALSE
	)

library(car)		
scatterplotMatrix(
	x = data[, c("Latitude", "Longitude", "Volume")], 
	reg.line = FALSE,
	col=c(3,
		"orangered",
		rgb(176/256, 196/256, 222/256, alpha=0.5)
		), 
	pch=19,
	lwd=3
	)

library(ggplot2)
library(GGally)
ggpairs(
	data[, c("Latitude", "Longitude", "Volume")]
	)

### Beanplot
data_LA_SF <- data[ which(data$County %in% c("Los Angeles", "San Francisco")), ]

library(beanplot)
beanplot(
	Volume ~ as.factor(County), 
	data = data_LA_SF, 
	side = "both", 
	col = list( grey(0.5),grey(0.8) )
	)

library(beanplot)
op <- par(las=2)
beanplot(
	Volume ~ as.factor(County), 
	data = data_LA_SF, 
	xlab = "",
	log = "y",
	side = "both", 
	col = list( c( grey(0.5), "white"), grey(0.8) ), 
	border = NA, 
	overallline = "median", 
	ll = 0.005
	)
legend(
	x = "bottomleft",
	fill=c( grey(0.5), grey(0.8) ), 
	legend=c( "LA", "SF" )
	)
par(op)