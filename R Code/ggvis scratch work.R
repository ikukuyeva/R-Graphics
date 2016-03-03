# Order of how you load plyr and dplyr matter per http://stackoverflow.com/questions/21653295/dplyr-issues-when-using-group-bymultiple-variables
# Ram and ManneR: 
detach(package:plyr) 


library(ggvis)
dropdown <- input_select( 
  c(
    "Red" = "darkred",
    "Blue" = "blue4",
    "Yellow" = "darkgoldenrod2"
    ),
  label = "Point color"
)

tracts.points %>%
  ggvis(~long, ~lat) %>%
  group_by(group, id) %>%
  layer_paths(strokeOpacity:=0.5, stroke:=grey(0.5)) %>%
  layer_points(
    data=df_hosp, 
    x=~Longitude, 
    y=~Latitude, 
    size:=12,
    fill:=dropdown) %>%
  hide_legend("fill") %>%
  set_options(width=400, height=400, keep_aspect=TRUE)

  
  add_tooltip(function(data) data$Hospital.Name) %>%