#test_util.R
library(testthat)
library(blissR)







test_that("substrRight", {
  #************************************************
  #TEST substrRight
  #************************************************
  aText <- "123456789"
  len <- 3
  substrRight_res <- substrRight(aText, len)
  substrRight_expRes <- "789" 
  
  expect_true(nchar(substrRight_res) == 3)
  expect_true(substrRight_res == substrRight_expRes)
  
  #************************************************
  #TEST substrRight
  #************************************************
  #timeWindowStart <- "2012-10-28 12:00:00 CET"
  #timeWindowEnd <- "2013-10-27 12:00:00 CET"
  
  #timeInterval <- c(as.POSIXlt(timeWindowStart), as.POSIXlt(timeWindowEnd))
  #getYearList_res <- getYearList(timeInterval[1], timeInterval[2])
  #timeInterval_expRes <- c(2012, 2013)
  
  #expect_true(length(getYearList_res) == 2)
  #expect_true(all(timeInterval_expRes == getYearList_res))



})