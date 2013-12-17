#test_ModisProcessor.R
library(testthat)
library(blissR)


resultFolder <- "/mnt/lun0/csv4scidb"
#files <- list.files(path = localArcPath, pattern = "*.hdf", full.names = TRUE, recursive = TRUE, include.dirs = TRUE)[1:2]




#modisCollections <- "5"
#requestedTiles <- "h10v08 "
#requestedProducts <- "MOD09Q1"
#timeWindowStart <- "2013-01-01"
#timeWindowEnd <- "2013-11-16"
#md <- new("ModisDownloader", timeWindowStart = timeWindowStart, timeWindowEnd = timeWindowEnd, requestedTiles = requestedTiles, requestedProducts = requestedProducts, collections = modisCollections)
#filepathsAmazon <- downloadRegionHdfs(md, "amazon")

#files <- unlist(filepathsAmazon)[c(1:48)]
#mp <- new("ModisProcessor", files, resultFolder)

#Test getters
#expect_that(getResultFolder(mp) == resultFolder, is_true())
#expect_that(all.equal(getFiles(mp), files), is_true())


#process(mp)



