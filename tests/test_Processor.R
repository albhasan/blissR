#test_Processor.R
library(testthat)
library(blissR)



#files <- list.files(path = "/mnt/lun0/MODIS_ARC/MODIS/MOD09Q1.005/2013.11.17", full.names = TRUE)
#files <- list.files(path = "/mnt/lun0/MODIS_ARC/MODIS/MOD09Q1.005", full.names = TRUE)[13:57]
t1 <- Sys.time()
resultFolder <- "/mnt/lun0/csv4scidb"
#p <- new ("Processor", files, resultFolder)
#process(p)
t2 <- Sys.time()
t2 - t1
#16.49702 mins processing 15 files (5 MOdis files)
#5.360787 mins processing 3 files (1 MOdis files)