#test_Util.R
library(testthat)
library(blissR)

u <- new("Util")


#substrRight
x <- "123456789"
n <- 3
res <- "789"
expect_that(substrRight(u, x = x, n = n) == res, is_true())


#getFileExtension
fileName <- "/mnt/lun0/MODIS_ARC/MODIS/MOD09Q1.005/2013.11.17/MOD09Q1.A2013321.h11v10.005.2013332222705.hdf"
res <- "hdf"
expect_that(getFileExtension(u, fileName = fileName) == res, is_true())


#getFilenameFromFilepath
filepath <- "/mnt/lun0/MODIS_ARC/MODIS/MOD09Q1.005/2013.11.17/MOD09Q1.A2013321.h11v10.005.2013332222705.hdf"
res <- "MOD09Q1.A2013321.h11v10.005.2013332222705.hdf"
expect_that(getFilenameFromFilepath(u, filepath = filepath) == res, is_true())


#processTime
res <- 20140102
time1 <- "2014/01/02"
time2 <- "2014002"
expect_that(processTime(u, time = time1) == res, is_true())
expect_that(processTime(u, time = time2) == res, is_true())


#getTimeFromHdfFilename
hdfFilename <- "MOD09Q1.A2013321.h11v10.005.2013332222705.hdf"
res <- "2013321"
expect_that(getTimeFromHdfFilename(u, hdfFilename) == res, is_true())


#getFileresultFromFilename
fileName <- "MOD09Q1.A2013321.h11v10.005.2013332222705.hdf"
band <- "1"
ext <- ".txt"
res <- "MOD09Q1.A2013321.h11v10.005.2013332222705band1.txt"
expect_that(getFileresultFromFilename(u, fileName = fileName, band = band, ext = ext) == res, is_true())


#getFileNoExtension
filename <- "MOD09Q1.A2013321.h11v10.005.2013332222705.hdf"
res <- "MOD09Q1.A2013321.h11v10.005.2013332222705"
expect_that(getFileNoExtension(u, filename) == res, is_true())

#getTileIdFromFilename
fileName <- "MOD09Q1.A2013321.h11v10.005.2013332222705.hdf"
res <- "h11v10"
expect_that(getTileIdFromFilename(u, fileName = fileName) == res, is_true())


#getFirstGmip
modisTileId1 <- "h00v00"#"h13v10"
modisTileId2 <- "h35v17"
nrows <- 4800
ncols <- 4800
res1 <- c(0,0)
res2 <- c(168000, 81600)
expect_identical(getFirstGmip(u, modisTileId = modisTileId1, nrows = nrows, ncols = ncols), res1)
expect_identical(getFirstGmip(u, modisTileId = modisTileId2, nrows = nrows, ncols = ncols), res2)

#text2date
dateAsText <- c("2014/01/02", "2014.01.02", "2014-01-02", "20140102", "2014002")
res <- as.POSIXlt(dateAsText[1])
for(d in dateAsText){
  expect_that(text2date(u, d) == res, is_true())
}


#date2ydoy
d1 <- "2013/01/02"
res1 <- "2013002"
expect_that(date2ydoy(u, d1) == res1, is_true())
d2 <- "2013/02/01"
res2 <- "2013032"
expect_that(date2ydoy(u, d2) == res2, is_true())
d3 <- "2002/03/01"
res3 <- "2002060"
expect_that(date2ydoy(u, d3) == res3, is_true())
d4 <- "2012/03/01"
res4 <- "2012061"
expect_that(date2ydoy(u, d4) == res4, is_true())
d5 <- "2012/12/31"
res5 <- "2012366"
expect_that(date2ydoy(u, d5) == res5, is_true())
d6 <- "2013/12/31"
res6 <- "2013365"
expect_that(date2ydoy(u, d6) == res6, is_true())


