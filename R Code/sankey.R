### Install package per http://ramnathv.github.io/rCharts/ and tutorial: http://rcharts.io/howitworks/
require(devtools)
install_github('rCharts', 'ramnathv')

library(rCharts)

### Per http://timelyportfolio.blogspot.co.uk/2013/07/all-my-roads-lead-back-to-financepimco.html

holdings = read.delim("http://timelyportfolio.github.io/rCharts_d3_sankey/holdings.txt", skip = 3, header = FALSE, stringsAsFactors = FALSE)
colnames(holdings) <- c("source","target","value")

#get rid of holdings with 0 weight or since copy/paste from Excel -
holdings <- holdings[-which(holdings$value == "-"),]
holdings$value <- as.numeric(holdings$value)

#now we finally have the data in the form we need
# sankeyPlot$setLib('.')
sankeyPlot <- rCharts$new()
sankeyPlot$setLib('http://timelyportfolio.github.io/rCharts_d3_sankey')

sankeyPlot$set(
  data = holdings,
  nodeWidth = 15,
  nodePadding = 10,
  layout = 32,
  width = 750,
  height = 500,
  labelFormat = ".1%"
)

sankeyPlot

### EX 2 (works):
test <- cbind.data.frame(sample(letters, 10), 
						 sample(letters, 10), 
						 runif(10)
						 )
colnames(test) <- c("source","target","value")

sankeyPlot2 <- rCharts$new()
sankeyPlot2$setLib('http://timelyportfolio.github.io/rCharts_d3_sankey')
sankeyPlot2$set(
  data = test,
  nodeWidth = 10,   # width of color
  nodePadding = 10, # spacing between nodes
  width = 750,
  height = 500,
  labelFormat = ".1%"
)
sankeyPlot2

# Ex 3: Titanic
df_Titanic <- as.data.frame(Titanic)
test <- df_Titanic[ c("Sex", "Class", "Survived", "Freq")]
test$Freq <- test$Freq/sum(test$Freq)
colnames(test) <- c("Sex", "source", "target","value")

test[ order(test$source), ]

library(sqldf)
tmp <- sqldf("select Sex, source, target, sum(value) as value
			   from test
			   group by Sex, source, target
			   order by source"
			   )

sankeyPlot3 <- rCharts$new()
sankeyPlot3$setLib('http://timelyportfolio.github.io/rCharts_d3_sankey')
sankeyPlot3$set(
  data = test,
  nodeWidth = 10,   # width of color
  nodePadding = 10, # spacing between nodes
  layout = 32,
  width = 750,
  height = 500,
  labelFormat = ".1%"
)
sankeyPlot3

### TODO: check numbers/flows