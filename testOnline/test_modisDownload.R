#test_modisDownload.R
require(testthat)
require(blissR)

#************************************************
#PARAMETERS
#************************************************
timeWindowStart <- "2012-10-28 12:00:00 CET"
timeWindowEnd <- "2013-10-27 12:00:00 CET"
tiles <- amazonTiles
product <- c("MOD09Q1")
#************************************************



#modisDownload(timeInterval, tiles, product)