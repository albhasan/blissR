#test_ModisProcessor.R
library(testthat)
library(blissR)


resultFolder <- "/mnt/lun0/csv4scidb"
localArcPath <- "/mnt/lun0/MODIS_ARC/MODIS"
files <- list.files(path = localArcPath, pattern = "*.hdf", full.names = TRUE, recursive = TRUE, include.dirs = TRUE)[1:5]
filePath <- "/mnt/lun0/MODIS_ARC/MODIS/MOD09Q1.005/2013.01.01/MOD09Q1.A2013001.h10v08.005.2013017013445.hdf"


#mp <- new("ModisProcessor", files, resultFolder)

#Test getters
#expect_that(getResultFolder(mp) == resultFolder, is_true())
#expect_that(all.equal(getFiles(mp), files), is_true())


#process(mp)