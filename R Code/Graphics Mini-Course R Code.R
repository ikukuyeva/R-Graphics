# 03/03/16
# Author: Irina Kukuyeva
# Graphics Mini-Course R Code

### Step 0: Load required packages
library(beanplot)
library(car)
library(ggplot2)
library(lattice)
library(maps)
library(maptools)
library(rgdal) 
library(plyr)   
library(dplyr)

#---------------------------------
# --- Data set
#---------------------------------
df <- read.table(
  "Number_of_Selected_Inpatient_Medical_Procedures__California_Hospitals__2005-2014.csv",
  sep = ",",
  header=TRUE,
  stringsAsFactors = FALSE
  )
names(df)[1] = 'Year'

### Step 1: Remove any 'statewide' counts
data_clean <- df %>%
  filter( County != "STATEWIDE" )

data_LA_SF <- data_clean %>%
  filter( 
    (County == "Los Angeles") | 
    (County == "San Francisco") 
  ) 

df_summary <- data_clean %>%
  dplyr::group_by(Year) %>%
  dplyr::summarise(Total.for.Year = sum(Volume, na.rm=TRUE))

### Step 1: Aggregate data to be at hospital level:
df_hosp <- data_clean %>%
  dplyr::group_by( Latitude, Longitude ) %>%
  dplyr::summarise( Total.for.Hospital = sum(Volume, na.rm=TRUE) )
	
### Step 2: Recode 'Volume' to have 2 categories only: high ( >100 cases ) and low ( <= 100 cases ):
df_hosp$volume_ind <- ifelse( 
 test = df_hosp$Total.for.Hospital > 100, 
 yes = 2, 
 no = 1 )

### Step 2: Read in shapefile and add latitude and longitude cordinates to it:
tracts = spTransform(
	readOGR(
		file.path("cb_2014_06_tract_500k"), 
		layer = "cb_2014_06_tract_500k"
		), 
	CRS("+proj=longlat +datum=WGS84")
	) 

### Step 2: Preprocess data ahead of plotting
tracts@data$id = rownames(tracts@data)
tracts.points = fortify(tracts, region="id")
tracts.df = plyr::join(
tracts.points, 
tracts@data, 
by="id"
)


#---------------------------------
# --- Plots of Counts
#---------------------------------
# Histograms
	# 1.
hist(
  df$Volume, 
  breaks=1000
  )

	# 2.

### Step 2: Visualize
hist(data_clean$Volume,
  xlab="Volume",
  main=""
  )

# Beanplot
op <- par(las=2)   # orient y-axis labels
beanplot(
  Volume ~ as.factor(County), 
  data = data_LA_SF, 
  xlab = "",
  log = "y",
  side = "both", 
  col = list( c( grey(0.5), "white"), grey(0.8) ), 
  border = NA, 
  overallline = "median", 
  ll = 0.005,
  show.names=FALSE
  )
legend(
  x = "bottomleft",
  fill=c( grey(0.5), grey(0.8) ), 
  legend=c( "LA", "SF" )
  )
par(op)

# Scatterplot
scatterplotMatrix(
  x = df[, c("Latitude", "Longitude", "Volume")], 
  reg.line = FALSE,
  col=c(3,
    "orangered",
    rgb(176/256, 196/256, 222/256, alpha=0.5)
    ), 
  pch=19,
  lwd=3
  )



#---------------------------------
### TS Plots
#---------------------------------
# Univariate


# Multivariate
	# 1.

	#2.

	#3.

#---------------------------------
### Geo-Plots
#---------------------------------
# 1.
plot(
 x = df_clean$Longitude, 
 y = df_clean$Latitude,
 xlab = "Longitude",
 ylab = "Latitude",
 pch = 16,
 col = rgb(176/256, 196/256, 222/256, alpha=0.5)
)
map("state", "california", add=TRUE)

# 2.

	### Step 3: Plot
	plot(
	 x = df_hosp$Longitude, 
	 y = df_hosp$Latitude, 
	 pch = 19, 
	 cex = df_hosp$volume_ind, 
	 col = df_hosp$volume_ind, 
	 xlab = "Longitude", 
	 ylab = "Latitude",
	 main="Indicator of overall volume between 2005-2014"
	)
	map("state", "california", add=TRUE)

	### Step 4: Add legend
	legend("topright", 
	 pch = 19, 
	 pt.cex = 1:2,
	 col = 1:2, 
	 c("Vol <= 100", "Vol > 100") 
	)
# 3.
 ### Step 3: Visualize
plot(tracts)

 ### Step 3: Visualize
ggplot() +
  geom_polygon(data = tracts.df,
               aes(x = long, y = lat, group = group),
               fill = grey(0.6), 
               color = grey(0.6), 
               alpha = 0.5
               ) + 
  geom_point(data = df_hosp,
             aes(x = Longitude, y = Latitude),
             color = "blue4", 
             alpha = 0.5, 
             shape = 3,
             size = 2 )
# 5.
library(ggvis)
tracts.points %>%
  ggvis(~long, ~lat) %>%
  group_by(group, id) %>%
  layer_paths(strokeOpacity:=0.5, stroke:=grey(0.5)) %>%
  layer_points(data=df_hosp, x=~Longitude, y=~Latitude, size:=8) %>%
  hide_legend("fill") %>%
  set_options(width=400, height=400, keep_aspect=TRUE)

# 6.
