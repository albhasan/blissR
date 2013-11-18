#test_ModisDownloader.R
library(testthat)
library(blissR)
#library(devtools)

baseUrl <- "ftp://ladsweb.nascom.nasa.gov/allData" 
argumentSeparator <- " "
modisCollections <- "5 6"
requestedTiles <- "h10v10 h11v10"
requestedProducts <- "MOD09Q1 MYD09Q1"
timeWindowStart <- "2012-10-20 12:00:00 CET"
timeWindowEnd <- "2012-10-29 12:00:00 CET"

#load_all()
#md <- new("ModisDownloader", timeWindowStart = "2012-10-20 12:00:00 CET", timeWindowEnd = "2012-10-29 12:00:00 CET", requestedTiles = "h10v10 h11v10", requestedProducts = "MOD09Q1 MYD09Q1", collections = "5 6", baseUrl = "ftp://ladsweb.nascom.nasa.gov/allData")
#a<-downloadHdfs(md)

