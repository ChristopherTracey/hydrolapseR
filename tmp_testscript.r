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




############


ts <- seq.POSIXt(as.POSIXct(lubridate::with_tz(startDate, "America/New_York"),'%m/%d/%y %H:%M'), as.POSIXct(lubridate::with_tz(endDate, "America/New_York"),'%m/%d/%y %H:%M'), by=900) # 900 is the number of seconds in 15min
#ts1 <- lubridate::with_tz(ts, "America/New_York")
#ts <- seq.POSIXt(as.POSIXlt("2001-09-01 0:00"), as.POSIXlt("2001-09-01 0:07"), by="min")
#ts <- format.POSIXct(ts,'%m/%d/%y %H:%M')

df <- data.frame(timestamp=ts)

data_with_missing_times <- dplyr::full_join(df,files2, by=c("timestamp"="mtime_rnd"))

getHydroData(siteNumber, graphtype, startDate, endDate, tz)

graphdata <- merge(dischargeUnit, data_with_missing_times, by.x="dateTime", by.y="timestamp") #, all.x=TRUE
# NOTE, probably should add something about all.x=true to properly format the graphs

# get the number of photos so we can properly pad the file names so things sort correctly...
padlength <- nchar(nrow(graphdata)) # used below in ggsave

graphdata <- graphdata[order(graphdata$dateTime),] 

df_gagemin <- min(floor(graphdata$Flow_Inst))
df_gagemax <- max(ceiling(graphdata$Flow_Inst))
df_datemin <- min(graphdata$dateTime)
df_datemax <- max(graphdata$dateTime)




exifinfo <- exifr::read_exif(as.character(graphdata$names[40]))  # read the exif data from the camera.
jpeg(filename="T:/HydrolapseRpackageDev/testOutput/fillimage1.jpg", width = exifinfo$ExifImageWidth, height = exifinfo$ExifImageHeight, res=72) # units = "px", 
dev.off()

for(i in 1:nrow(graphdata)){
  graphdata1 <- graphdata[1:i,]
  
  if(is.na(graphdata$names[i])){
    img <- jpeg::readJPEG("T:/HydrolapseRpackageDev/testOutput/fillimage1.jpg") 
  } else {
    img <- jpeg::readJPEG(graphdata$names[i])   
  }
  
  gpp <- grid::rasterGrob(img, interpolate=TRUE) 
  # gpp$width <- unit(1, "in") 
  # gpp$height <- unit(1, "in")
  a <- ggplot2::ggplot(graphdata1, ggplot2::aes(x=dateTime,y=Flow_Inst)) + 
    ggplot2::xlab("Date") +
    ggplot2::ylab("Flow (cfs)") +
    ggplot2::annotation_custom(gpp) +
    ggplot2::geom_line(color='red2', size=2) + 
    ggplot2::expand_limits(x=c(df_datemin,df_datemax),y=c(df_gagemin,df_gagemax)) +
    ggplot2::theme(
      panel.grid.major=ggplot2::element_blank(), 
      panel.grid.minor=ggplot2::element_blank(), 
      panel.background=ggplot2::element_blank(), 
      axis.line=ggplot2::element_line(colour = "black")
    ) +
    #theme(panel.border=element_blank(), panel.grid.major=element_blank(), panel.grid.minor=element_blank(), axis.line=element_line(colour="black"))
    ggplot2::ggsave(filename = paste(outputDirectory,"output", paste0("photo", stringr::str_pad(i, padlength, pad="0"),".jpg"),sep = "/"))
  print(paste("photo", i, "of", nrow(df), "saved"), sep=" ")
}

# make a movie
library(av)
outputfiles <- list.files(paste(outputDirectory,"output", sep="/"), recursive=TRUE, pattern="jpg", full.names=TRUE)
av_encode_video(outputfiles, output=paste(outputDirectory, "/testvideo.mp4", sep=""), framerate=12)






