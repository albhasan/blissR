#test_Constant.R
library(testthat)
library(blissR)


res <- "R es un pedazo de mierda"
c <- new("Constant", path2scidbBin = res)

expect_that(getPath2scidbBin(c) == res, is_true())

