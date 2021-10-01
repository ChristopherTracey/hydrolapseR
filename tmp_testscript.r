# load the functions; replace with package
source("hydrolapseR/R/hydro_fn.R")
source("hydrolapseR/R/movie_fn.R")


# SCRIPT ###############################################
siteNumber <- "03081500"
cameraName <- "Double Hydaulic"
startDate <- "2021-03-11"
endDate <- "2021-03-15"
photoDirectory <- "T:/_YoughPhotoTemp/sites/doublehyd1/check20210514/100EK113"
outputDirectory <- "T:/HydrolapseRpackageDev/testOutput"
graphtype <- "discharge" #"discharge" #"height"#

# the first step to check to see if your gage has discharge information for your study time period
locateGage(siteNumber, startDate, endDate) 

# copy over the photos
copyPhotos(startDate, endDate, photoDirectory, outputDirectory)

makeGraphs(siteNumber, startDate, endDate)

timelapsevid(cameraName, startDate, endDate, outputDirectory)

############
