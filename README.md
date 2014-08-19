blissR - PROJECT ABANDONED
======

This package constains R scripts for the BLISS project.

DEPENDENCIES
This package depends on other R Packages:
-CURL. In Ubuntu, open a terminal and type:
--sudo apt-get install curl
--sudo apt-get install libcurl4-openssl-dev
-In R, type
-install.packages("RCurl")
-install.packages("testthat")
-install.packages("MODIS", repos="http://R-Forge.R-project.org")

INSTALLATION
To install this package, build/download the TAR.GZ file and run
-install.packages("~/Downloads/blissR_0.1.tar.gz", repos = NULL, type="source")

USE
#Create an object
md <- new("ModisDownloader", timeWindowStart = "2012-10-20 12:00:00 CET", timeWindowEnd = "2012-10-29 12:00:00 CET", requestedTiles = "h10v10 h11v10", requestedProducts = "MOD09Q1 MYD09Q1", collections = "5 6", baseUrl = "ftp://ladsweb.nascom.nasa.gov/allData")
#Call the download function. The variable a contains the path to the downloaded files
a <- downloadHdfs(md)
