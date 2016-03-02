library(dplyr)
library(lattice)
library(ggplot2)
# library(stringr)

df <- read.csv(
  file = "Number_of_Selected_Inpatient_Medical_Procedures__California_Hospitals__2005-2014.csv",
  encoding='UTF-8',
  stringsAsFactors = FALSE
  )
# Per http://stackoverflow.com/questions/33264688/importing-csv-files-with-special-characters
# tmp <- gsub("\u0096", "", df$Hospital.Name, fixed=TRUE)

names(df)[1] = 'Year'
# Or directly from https://chhs.data.ca.gov/api/views/mdt8-gwyw/rows.csv?accessType=DOWNLOAD&bom=true

### Checking all available encodings to see if dash can be rendered properly; no
### -> emailed owner of data set to mention this.
# for(e in iconvlist()) {
# print(e)
# print(read.csv(
#   file = "Number_of_Selected_Inpatient_Medical_Procedures__California_Hospitals__2005-2014.csv",
#   encoding='UTF-8',
#   stringsAsFactors = FALSE
#   )$Hospital.Name[10])
# print("---")
# Sys.sleep(1)
# }

### Check how dash is rendered using a given encoding:
# x <- (read.csv(
#   file = "Number_of_Selected_Inpatient_Medical_Procedures__California_Hospitals__2005-2014.csv",
#   encoding='UTF-8',
#   stringsAsFactors = FALSE
#   )$Hospital.Name[10])

df_clean <- df %>%
  filter( County != "STATEWIDE" )

df_LA_CABG <- df %>%
  filter( (County == "Los Angeles") & (Procedure == "CABG") & (Volume > 0) )
## Alternatively:
# filter(df, (County == "Los Angeles") & (Procedure == "CABG")  & (Volume > 0) )

df_Cedars <- df_clean %>%
	filter( Hospital.Name == 'Cedars Sinai Medical Center')

# sort(unique(df_clean$County))

### Across hospitals of LA:
xyplot(Volume ~ Year | Hospital.Name, 
	data=df_LA_CABG,
	par.strip.text=list(cex=0.5),
	type="b",
	main="Coronary artery bypass grafting (CABG) in LA"
	)

### For one hospital:
ggplot( data = df_Cedars ) + 
  geom_line(
  	aes(x = Year, y = Volume, linetype = Procedure)
  	) +
  scale_x_continuous(
    breaks=seq(2005, 2014, 3)
    ) +
  theme_classic() +
  ggtitle("Volume of Procedures for Cedars Sinai Medical Center")
  
### Exercise II: How many procedures (overall) per year:
data_clean %>%
  group_by(Year, Procedure) %>%
  summarise(Total.for.Year = sum(Volume, na.rm=TRUE))

