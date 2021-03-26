

# make a movie
library(av)
outputfiles <- list.files(here::here("data", "sites", sitename, "output"), recursive=TRUE, pattern="jpg", full.names=TRUE)
av_encode_video(outputfiles, output=here::here("data", "sites", sitename, "output", paste(sitename, "_20200601.mp4", sep="")), framerate=12)
