#test_ModisDownloader.R
library(testthat)
library(blissR)


#baseUrl <- "ftp://ladsweb.nascom.nasa.gov/allData" 
argumentSeparator <- " "
modisCollections <- "5 6"
requestedTiles <- "h10v08 h11v09"
requestedProducts <- "MOD09Q1 MYD09Q1"
timeWindowStart <- "2013-01-01"
timeWindowEnd <- "2013-01-02"


#md <- new("ModisDownloader", timeWindowStart = "2013-01-01", timeWindowEnd = "2013-01-02", requestedTiles = "h10v10 h11v10", requestedProducts = "MOD09Q1 MYD09Q1", collections = "5 6")
#filepaths <- downloadHdfs(md)

#md <- new("ModisDownloader", timeWindowStart = "2013-01-01", timeWindowEnd = "2013-01-02", requestedTiles = "h10v10 h11v10", requestedProducts = "MOD09Q1 MYD09Q1", collections = "5 6")
#filepaths <- downloadHdfs(md)
#filepathsAmazon <- downloadRegionHdfs(md, "amazon")