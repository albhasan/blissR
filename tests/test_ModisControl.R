#test_ModisControl

library("RMySQL")

host <- "gis-obama.uni-muenster.de"
dbname <- "modisbliss"
user <- "bliss"
#password <- 
modisFiles <- c("img1", "img2")
isLoaded2tmp <- c(0,0)
isLoaded <- c(1,1)