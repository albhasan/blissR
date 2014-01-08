#test_ModisGrid.R
library(testthat)
library(blissR)


spaceResolution = 1 
timeOrigin = '2012/01/01'
timeResolution = 8

mg <- new("ModisGrid", spaceResolution = spaceResolution, timeOrigin = timeOrigin, timeResolution = timeResolution)

#date2grid
dateAsText <- c("2013/12/27", "2013.12.27", "2013-12-27", "2011.01.01", "2012.02.18", "2012.03.05", "2013.01.25", "2013.04.07", "2013.07.12")
for(d in dateAsText){
  expect_that(is.na(date2grid(mg, d)), is_false())  
}

i0 <- "2012/01/01"
expect_that(date2grid(mg, i0) == 0, is_true())
i1 <- "2012/01/09"
expect_that(date2grid(mg, i1) == 1, is_true())
i2 <- "2012/01/17"
expect_that(date2grid(mg, i2) == 2, is_true())
i45 <- "2012/12/26"
expect_that(date2grid(mg, i45) == 45, is_true())
i46 <- "2013/01/01"
expect_that(date2grid(mg, i46) == 46, is_true())





spaceResolution = 1 
timeOrigin = '2002/01/01'
timeResolution = 8

mg <- new("ModisGrid", spaceResolution = spaceResolution, timeOrigin = timeOrigin, timeResolution = timeResolution)

i0 <- "2002/01/01"
expect_that(date2grid(mg, i0) == 0, is_true())
i1 <- "2002/01/09"
expect_that(date2grid(mg, i1) == 1, is_true())
i2 <- "2002/01/17"
expect_that(date2grid(mg, i2) == 2, is_true())
i45 <- "2002/12/27"
expect_that(date2grid(mg, i45) == 45, is_true())
i46 <- "2003/01/01"
expect_that(date2grid(mg, i46) == 46, is_true())
i92 <- "2004/01/01"
expect_that(date2grid(mg, i92) == 92, is_true())
i138 <- "2005/01/01"
expect_that(date2grid(mg, i138) == 138, is_true())


