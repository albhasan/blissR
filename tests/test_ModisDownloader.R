#test_ModisDownloader.R
library(testthat)
library(blissR)


modisCollections <- "5"
requestedTiles <- "h10v08 "
requestedProducts <- "MOD09Q1"
timeWindowStart <- "2013-01-01"
timeWindowEnd <- "2013-12-31"


md <- new("ModisDownloader", timeWindowStart = timeWindowStart, timeWindowEnd = timeWindowEnd, requestedTiles = requestedTiles, requestedProducts = requestedProducts, collections = modisCollections)

#Test getters
expect_that(getTimeWindowStart(md) == timeWindowStart, is_true())
expect_that(getTimeWindowEnd(md) == timeWindowEnd, is_true())
expect_that(getRequestedTiles(md) == requestedTiles, is_true())
expect_that(getRequestedProducts(md) == requestedProducts, is_true())
expect_that(getCollections(md) == modisCollections, is_true())

#filepaths <- downloadHdfs(md)
#filepathsAmazon <- downloadRegionHdfs(md, "amazon")
#filepathsSouthAmerica <- downloadRegionHdfs(md, "southamerica")


#TEST THE FILES
#allhdfs <- list.files(path = "/mnt/lun0/MODIS_ARC/MODIS/", full.names = TRUE, recursive = TRUE)
#res <- .findInvalidHdfs(allhdfs)





