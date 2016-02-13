### Per http://zevross.com/blog/2015/10/14/manipulating-and-mapping-us-census-data-in-r-using-the-acs-tigris-and-leaflet-packages-3/#census-data-the-easyer-way
### Get API key: http://api.census.gov/data/key_signup.html
library(tigris)
library(acs)

api.key <- scan("C:/Users/ikukuyeva/Documents/Misc/Census_api_key.apikey", what="character")
api.key.install(key=api.key)

tracts <- tracts(state = 'CA', cb=TRUE)
geo<-geo.make(state=c("CA"),
			  county="*",
              tract="*"
              )
income<-acs.fetch(endyear = 2014, 
				  span = 5, 
				  geography = geo,
                 table.number = "B19001", 
                 col.names = "pretty"
                 )


### Yelp search API:
# https://www.yelp.com/developers/documentation/v2/search_api
# -> Get data from Yelp near the location of meetup and plot on shapefile
