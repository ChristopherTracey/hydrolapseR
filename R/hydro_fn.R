

######################################################################################################################
#' function to find the local gage
#'
#' This function loads a file as a matrix. It assumes that the first column
#'
#' @param siteNumber Path to the input file
#' @param startDate
#' @param endDate
#' @export
locateGage <- function(siteNumber, startDate, endDate){
  # check the dates to see if they are properly formatted
  startDate <- lubridate::parse_date_time(startDate, orders="ymd")
  endDate <- lubridate::parse_date_time(endDate, orders="ymd")
  if(endDate > Sys.Date()){
    cat(paste(endDate, " is beyond the current date.", sep=""))
  } else {
    cat()
  }
  
  siteINFO <<- dataRetrieval::readNWISsite(siteNumber)
  
  cat(paste("The selected site is ", siteINFO$station_nm," (",siteINFO$site_no,").", sep=""))
  
  # check to see that the gage has data within your study window
  whatData <<-dataRetrieval::whatNWISdata(siteNumber=siteNumber, parameterCd=c("00060","00065"), service="uv") # 00060 = discharge/flow; 00065 = gage height (feet)
  for(i in 1:nrow(whatData)){
    pcode <- ifelse(whatData$parm_cd[i]=="00060","Flow","Gage height")
    if(whatData$begin_date[i]<startDate&whatData$end_date[i]>endDate){
      cat(paste(pcode, " (", whatData$parm_cd[i],") " ,"data is available for the range of your study period\n",sep=""))
    } else if(whatData$begin_date[i]<startDate&whatData$end_date[i]<endDate) {
      cat(paste(pcode, " (", whatData$parm_cd[i],") " ,"data is not available for the end of your study period\n",sep=""))
    } else if(whatData$begin_date[i]>startDate&whatData$end_date[i]>endDate) {
      cat(paste(pcode, " (", whatData$parm_cd[i],") " ,"data is not available for the beginning of your study period\n",sep=""))
    } else {
      cat(paste(pcode, " (", whatData$parm_cd[i],") " ,"data is not available for any portion of your study period from this gage\n",sep=""))
    }
    
  }
  
    # look up timezone of the gage
  tz <<- lutz::tz_lookup_coords(siteINFO$dec_lat_va, siteINFO$dec_long_va, method="accurate", warn=FALSE)
  
  # HOW do we find the closest Upstream gage????
  
}


#' function to get gage data
#'
#' This function loads a file as a matrix. It assumes that the first column
#' contains the rownames and the subsequent columns are the sample identifiers.
#' Any rows with duplicated row names will be dropped with the first one being
#' kepted.
#'
#' @param siteNumber Path to the input file
#' @param parameterCd
#' @param startDate
#' @param endDate
#' @param tz timezone to report results in. this defaults to the timezone where the gage is located unless otherwise specified.
#' @return A matrix of the infile
#' @export
getHydroData <- function(siteNumber,graphtype, startDate, endDate, tz){
  # just to set up some variables
  if(graphtype=="discharge"){
    parameterCd <- "00060"  # Discharge
    flowVar <- "Flow_Inst"
    labely <- "Discharge (cfs)"
  } else if(graphtype=="height"){
    parameterCd <- "00065" # gage height
    flowVar <- "GH_Inst"
    labely <- "Gage Height (feet)"
  } else {
    cat("please enter a valid graph type")
  }
  # get flow/height records
  dischargeUnit <- dataRetrieval::readNWISuv(siteNumber, parameterCd, startDate, endDate, tz)
  dischargeUnit <- dataRetrieval::renameNWISColumns(dischargeUnit)
  #dischargeUnit$dateTime <- gsub('.{4}$', '',  dischargeUnit$dateTime) # remove the timzone
  #dischargeUnit$dateTime <- lubridate::date(dischargeUnit$dateTime) # remove the timzone
  dischargeUnit <<- dischargeUnit

  # this produces a hydrograph to examine to make sure we're looking at the right data
  ggplot2::ggplot(dischargeUnit, ggplot2::aes_string(x="dateTime", y=flowVar)) +
    ggplot2::ggtitle(paste(siteINFO$station_nm," (",siteINFO$site_no,")", sep="")) +
    ggplot2::xlab("Date") +
    ggplot2::ylab(labely) +
    ggplot2::geom_line(color='red2', size=1.5) +
    ggplot2::expand_limits(x=c( min(dischargeUnit$dateTime), max(dischargeUnit$dateTime)),y=c( min(floor(dischargeUnit[[flowVar]])),max(ceiling(dischargeUnit[[flowVar]])) )) +
    ggplot2::theme(
      panel.grid.major=ggplot2::element_blank(),
      panel.grid.minor=ggplot2::element_blank(),
      panel.background=ggplot2::element_blank(),
      axis.line=ggplot2::element_line(colour = "black")
    )

}

# library(dplyr)
# dischargeUnit %>%
#   tidyr::gather(indicator, percentage, c(4:5), -c(1:3, 6)) %>%
#   ggplot2::ggplot(ggplot2::aes(date, percentage, colour = indicator)) + 
#   ggplot2::geom_line(size=1, ggplot2::aes(linetype = lubridate::year(date) >= 2000)) +
#   ggplot2::scale_linetype(guide = F)


