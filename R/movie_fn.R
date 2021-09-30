

######################################################################################################################
#' function to copy the needed to a directory
#'
#' This function...
#'
#' @param startDate
#' @param endDate
#' @param photoDirectory
#' @param outputDirectory
#' @export
copyPhotos <- function(startDate, endDate, photoDirectory, outputDirectory){
  ifelse(!dir.exists(paste(outputDirectory,"srcphoto", sep="/")), dir.create(paste(outputDirectory,"srcphoto", sep="/")), FALSE)
  files <- list.files(photoDirectory, recursive=TRUE, pattern="JPG", full.names=FALSE)
  print(paste(length(files),"image files were found in the",photoDirectory, "directory.", sep=" "))
  files1 <- file.info(paste(photoDirectory,files, sep="/"))
  files <- cbind(files, files1)
  files$names <- rownames(files)
  files$names <- as.character(files$names)
  rm(files1)
  files$minutes <- lubridate::minute(files$mtime)
  files$mtime_rnd <- lubridate::round_date(files$mtime, unit="minute")
  files$minutes_rnd <- lubridate::minute(files$mtime_rnd)
  files <- files[which(files$mtime_rnd>=as.POSIXct(startDate)&files$mtime_rnd<=as.POSIXct(endDate)),]
  files$suit[files$minutes_rnd==0|files$minutes_rnd==15|files$minutes_rnd==30|files$minutes_rnd==45] <- "suitable"
  files2 <- files[which(files$suit=="suitable"),]
  files2 <- files2[!duplicated(files2[,c('mtime_rnd')]),] # eliminate any duplicate records
  file.copy(from=paste(photoDirectory,files2$files, sep="/"), to=paste0(outputDirectory,"/srcphoto"), recursive=FALSE, copy.mode=TRUE)   
}

######################################################################################################################
#' function to make a movie
#'
#' This function...
#'
#' @param siteNumber Path to the input file
#' @param cameraName
#' @param startDate
#' @param endDate
#' @param photoDirectory
#' @export
timelapsevid <- function(siteNumber, startDate, endDate, photoDirectory){
  # make a movie
  nameVideo <-
  outputfiles <- list.files(paste(outputDirectory,"output", sep="/"), recursive=TRUE, pattern="jpg", full.names=TRUE)
  av::av_encode_video(outputfiles, output=paste(outputDirectory, "/", nameVideo,".mp4", sep=""), framerate=12)
}  
  



######################################################################################################################
#' function to make a movie
#'
#' This function...
#'
#' @param siteNumber Path to the input file
#' @param cameraName
#' @param startDate
#' @param endDate
#' @param photoDirectory
#' @export
  ts <- seq.POSIXt(as.POSIXct(lubridate::with_tz(startDate, "America/New_York"),'%m/%d/%y %H:%M'), as.POSIXct(lubridate::with_tz(endDate, "America/New_York"),'%m/%d/%y %H:%M'), by=900) # 900 is the number of seconds in 15min

  df <- data.frame(timestamp=ts)

  data_with_missing_times <- dplyr::full_join(df,files2, by=c("timestamp"="mtime_rnd"))

  getHydroData(siteNumber, graphtype, startDate, endDate, tz)

  graphdata <- merge(dischargeUnit, data_with_missing_times, by.x="dateTime", by.y="timestamp") #, all.x=TRUE

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



}




