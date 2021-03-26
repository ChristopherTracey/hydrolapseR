siteNumber <- "03081500"
graphtype <- "height"#"discharge" #"height"#
startDate <- "2020-01-01"
endDate <- "2021-03-15"
outputDirectory <- ""


locateGage(siteNumber) 

getHydroData(siteNumber, graphtype, startDate, endDate, tz)




