siteNumber <- "03081500"
startDate <- "2021-01-01"
endDate <- "2021-03-15"

source("hydrolapseR/R/hydro_fn.R")
#source("hydrolapseR/R/movie_fn.R")


outputDirectory <- ""

# the first step to check to see if your gage has discharge information for your study time period
locateGage(siteNumber, startDate, endDate) 

# select what type of graph you want to make
graphtype <- "discharge" #"discharge" #"height"#

getHydroData(siteNumber, graphtype, startDate, endDate, tz)




