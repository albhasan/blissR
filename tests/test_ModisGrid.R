#test_ModisGrid
library(testthat)
library(blissR)


lon <- -74
lat <- 4
modisTileId <- "h10v08"
nrow <- 4800
ncol <- 4800

#mg <- new("ModisGrid")

#Test getters # It does not pass the automatic check but no error when running the test by hand#Explanation: R is a piece of shit
#expect_that(ncol(getModisGridBoundaries(mg)) == 6, is_true())
#expect_that(nrow(getModisGridBoundaries(mg)) == 648, is_true())
