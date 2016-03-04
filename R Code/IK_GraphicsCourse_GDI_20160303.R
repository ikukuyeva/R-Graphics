# 03/03/16
# Author: Irina Kukuyeva
# Graphics Mini-Course R Code

# install.packages( c("lattice", "ggplot2") )
library(dplyr)
library(beanplot)
library(car) 
library(lattice)
library(ggplot2)
library(maps)
library(sp)
library(rgdal)
library(maptools)
library(plyr)
library(ggvis)
library(leaflet)

#---------------------------------
# --- Overview of R
#---------------------------------

### --- Functions
f <- function(x, y=1){
  answer <- x * 2 + y + 1
  return(answer)
}

f(2)        # 6
f(3, 2)     # 9
f(y=3, 2)   # 8
f(y=3, x=3) # 10

### --- Comparisons
# Suppose x and y are:
x = 3
y = 5

x == 3  # is x 3?
x != y  # are x and y different?
x >= y  # is x greater than or equal to y?
(x==3) & (y==4) # is it true that x is 3 and y is 4?
(x==3) | (y==4) # is it true that x is 3 or y is 4?

### --- Working with Data Frames:
filepath = "http://www.ats.ucla.edu/stat/data/test_missing_comma.txt"
### Other valid paths:
# filepath = "C:/Documents/test_missing_comma.txt"
# filepath = "./test_missing_comma.txt"

df <- read.table(
  file = filepath, 
  header = TRUE, 
  sep = ","
  )

head(df, 5)
tail(df, 7)
names(df)
dim(df)
head(df$gender)
unique(df$gender)
table(df$gender, useNA='always')
head( df[, c('gender', 'ses') ], 3) 
summary(df)

#---------------------------------
# --- Data Set
#---------------------------------

# Path to file: https://chhs.data.ca.gov/api/views/mdt8-gwyw/rows.csv?accessType=DOWNLOAD
df <- read.table(
  "Number_of_Selected_Inpatient_Medical_Procedures__California_Hospitals__2005-2014.csv",
  sep = ",",
  header=TRUE,
  stringsAsFactors = FALSE
  )
names(df)[1] = 'Year'

dim(df)

#---------------------------------
# --- Plots of Counts
#---------------------------------

### --- Histogram
hist(
  df$Volume, 
  breaks=1000
  )

# Potential outlier detection:
df %>%
  filter( Volume == max(Volume, na.rm=TRUE) )

# Remove any 'statewide' counts
data_clean <- df %>%
  filter( County != "STATEWIDE" )

hist(data_clean$Volume,
  xlab="Volume",
  main=""
  )

### --- Side-by-side histogram
data_LA_SF <- data_clean %>%
  filter( 
    (County == "Los Angeles") | 
    (County == "San Francisco") 
  ) 
op <- par(las=2)   # orient y-axis labels
beanplot(
  Volume ~ as.factor(County), 
  data = data_LA_SF, 
  xlab = "",
  log = "y",
  side = "both", 
  col =list( c(grey(0.5), "white"), grey(0.8) ), 
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

### --- Scatterplot

# v1 
scatterplotMatrix(
  x = df[, c("Latitude", "Longitude", "Volume")], 
  smoother = FALSE, 
  reg.line = FALSE
  )

# v2
?scatterplotMatrix # for documentation  
scatterplotMatrix(
  x = df[, c("Latitude", "Longitude", "Volume")], 
  reg.line = FALSE
  )

# v3    
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
# --- Time Series Plots
#---------------------------------

### --- Univariate
### Step 1: Get yearly counts across hospitals and procedures
df_summary <- data_clean %>%
  dplyr::group_by(Year) %>%
  dplyr::summarise(Total.for.Year = sum(Volume, na.rm=TRUE))

plot(
  x=df_summary$Year,
  y=df_summary$Total.for.Year,
  xlab="Year",
  ylab="Volume",
  type="b",
  main="Yearly Volume for 6 procedures in CA"
  )

### --- Multivariate 
# v1
df_LA_CABG <- df %>%
  filter( 
    (County == "Los Angeles") & 
    (Procedure == "CABG") & 
    (Volume > 0) 
    )

xyplot( Volume ~ Year | Hospital.Name, 
  data=df_LA_CABG,
  par.strip.text=list(cex=0.5),
  type="b",
  main="LA Coronary artery bypass grafting (CABG)"
  )

# v2
df_Cedars <- data_clean %>%
  filter( Hospital.Name == 'Cedars Sinai Medical Center')

ggplot( data = df_Cedars ) + 
  geom_line( aes(
    x = Year, 
    y = Volume, 
    linetype = Procedure) ) +
  scale_x_continuous( breaks=seq(
    from=2005, 
    to=2014, 
    by=3) ) +
  theme_classic() +
  ggtitle( "Volume of Procedures for Cedars Sinai Medical Center" )

#---------------------------------
# --- Geo Plots
#---------------------------------

### --- Static geo plots

# v1
plot(
 x = data_clean$Longitude, 
 y = data_clean$Latitude,
 xlab = "Longitude",
 ylab = "Latitude",
 pch = 16,
 col = rgb(176/256, 196/256, 222/256, alpha=0.5)
)
map("state", "california", add=TRUE)

# v2
df_hosp <- data_clean %>%
  dplyr::group_by( Latitude, Longitude ) %>%
  dplyr::summarise( Total.for.Hospital = sum(Volume, na.rm=TRUE) )
  
df_hosp$volume_ind <- ifelse( 
 test = df_hosp$Total.for.Hospital > 100, 
 yes = 2, 
 no = 1 
 )

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
legend("topright", 
 pch = 19, 
 pt.cex = 1:2,
 col = 1:2, 
 c("Vol <= 100", "Vol > 100") 
)

# v3
tracts = spTransform(
  readOGR(
    file.path("cb_2014_06_tract_500k"), 
    layer = "cb_2014_06_tract_500k"
    ), 
  CRS("+proj=longlat +datum=WGS84")
  ) 

plot(tracts) # plot shapefile

tracts@data$id = rownames(tracts@data)
tracts.points = fortify(tracts, region="id")
tracts.df = join(
 tracts.points, 
 tracts@data, 
 by="id"
 )

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

### --- Interactive geo plots

# v1
tracts.points %>%
  ggvis(~long, ~lat) %>%
  group_by(group, id) %>%
  layer_paths(strokeOpacity:=0.5, stroke:=grey(0.5)) %>%
  layer_points(data=df_hosp, x=~Longitude, y=~Latitude, size:=8) %>%
  hide_legend("fill") %>%
  set_options(width=400, height=400, keep_aspect=TRUE)

# v2
tracts.points %>%
  ggvis(~long, ~lat) %>%
  group_by(group, id) %>%
  layer_paths(
    strokeOpacity:=0.5, 
    stroke:=grey(0.5)) %>%
  layer_points(
    data=df_hosp, 
    x=~Longitude, 
    y=~Latitude, 
    size:=input_slider(8, 100, value = 12, label='Point size')) %>%
  hide_legend("fill") %>%
  set_options(width=400, 
    height=400, 
    keep_aspect=TRUE)

# v3
leaflet(data = df_hosp) %>% 
  addTiles() %>%
  addMarkers(
    ~Longitude, 
    ~Latitude, 
    popup = ~as.character(Total.for.Hospital)
    )