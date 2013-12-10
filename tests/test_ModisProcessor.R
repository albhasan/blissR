#test_ModisProcessor.R
library(testthat)
library(blissR)


resultFolder <- "/home/alber/csv2scidb"
localArcPath <- "/home/alber/MODIS_ARC/MODIS"
files <- list.files(path = localArcPath, pattern = "*.hdf", full.names = TRUE, recursive = TRUE, include.dirs = TRUE)

#md <- new("ModisDownloader", timeWindowStart = "2012-10-20 12:00:00 CET", timeWindowEnd = "2012-10-29 12:00:00 CET", requestedTiles = "h10v10 h11v10", requestedProducts = "MOD09Q1 MYD09Q1", collections = "5 6", baseUrl = "ftp://ladsweb.nascom.nasa.gov/allData")
#a<-downloadHdfs(md)

##mp <- new("ModisPrcessor", files)
#b<-processHdfs(mp) 