#' The MODISDOWNLOADER class
#'
#' Use this class for downloading MODIS tiles.
#'
#' You can specify MODIS's products, tiles, collections, satellites, and time interval.
#'
#'@section Slots: 
#'  \describe{
#'    \item{\code{timeWindowStart}:}{Object of class \code{"character"}, it is the start date of the time interval for selecting MODIS data}
#'    \item{\code{timeWindowEnd}:}{Object of class \code{"character"}, it is the end date of the time interval for selecting MODIS data}
#'    \item{\code{requestedTiles}:}{Object of class \code{"character"}, it is whitespace separated list of MODIS tiles to download}
#'    \item{\code{requestedProducts}:}{Object of class \code{"character"}, it is whitespace separated list of MODIS products to download}
#'    \item{\code{collections}:}{Object of class \code{"character"}, it is whitespace separated list of MODIS collections to download}
#'  }
#'
#' @note No notes
#' @name ModisDownloader 
#' @aliases ModisDownloader-class
#' @exportClass ModisDownloader
#' @author Alber Sanchez
setClass(
  Class = "ModisDownloader", 
  slots = c(timeWindowStart = "character", 
            timeWindowEnd = "character",
            requestedTiles = "character",
            requestedProducts = "character", 
            collections = "character"),
  validity = function(object){
    #cat("~~~ ModisDownloader: inspector ~~~ \n")
    res <- TRUE
    if(nchar(object@timeWindowStart) < 8)
      res <- FALSE
    if(nchar(object@timeWindowEnd) < 8)
      res <- FALSE
    if(nchar(object@requestedTiles) < 1)
      res <- FALSE
    if(nchar(object@requestedProducts) < 1)
      res <- FALSE
    if(nchar(object@collections) < 1)
      res <- FALSE
    if(res == FALSE)
      stop ("[ModisDownloader: validation] Some parameters are invalid")
    return(res)
  }
)

#*******************************************************
#CONSTRUCTOR
#*******************************************************
setMethod(
  f="initialize",
  signature="ModisDownloader",
  definition=function(.Object, timeWindowStart, timeWindowEnd, requestedTiles, requestedProducts, collections){
    #cat ("~~~~~ ModisDownloader: initializator ~~~~~ \n")
    .Object@timeWindowStart <- timeWindowStart
    .Object@timeWindowEnd <- timeWindowEnd
    .Object@requestedTiles <- requestedTiles
    .Object@requestedProducts <- requestedProducts
    .Object@collections <- collections
    validObject(.Object)# call of the inspector
    return(.Object)
  }
)

#*******************************************************
#ACCESSORS
#*******************************************************


#' Returns the object's start time
#' 
#' @docType methods
#' @param object A ModisDownloader object
#' @export 
setGeneric("getTimeWindowStart",function(object){standardGeneric ("getTimeWindowStart")})
setMethod("getTimeWindowStart","ModisDownloader",
          function(object){
            return(object@timeWindowStart)
          }
)

#' Returns the object's end time
#' 
#' @param object A ModisDownloader object
#' @docType methods
#' @export 
setGeneric("getTimeWindowEnd",function(object){standardGeneric ("getTimeWindowEnd")})
setMethod("getTimeWindowEnd","ModisDownloader",
          function(object){
            return(object@timeWindowEnd)
          }
)

#' Returns the object's requested tiles
#' 
#' @param object A ModisDownloader object
#' @docType methods
#' @export 
setGeneric("getRequestedTiles",function(object){standardGeneric ("getRequestedTiles")})
setMethod("getRequestedTiles","ModisDownloader",
          function(object){
            return(object@requestedTiles)
          }
)

#' Returns the object's requested products
#' 
#' @param object A ModisDownloader object
#' @docType methods
#' @export
setGeneric("getRequestedProducts",function(object){standardGeneric ("getRequestedProducts")})
setMethod("getRequestedProducts","ModisDownloader",
          function(object){
            return(object@requestedProducts)
          }
)

#' Returns the object's requested collections
#' 
#' @param object A ModisDownloader object
#' @docType methods
#' @export
setGeneric("getCollections",function(object){standardGeneric ("getCollections")})
setMethod("getCollections","ModisDownloader",
          function(object){
            return(object@collections)
          }
)


#*******************************************************
#GENERIC METHODS
#*******************************************************

#*******************************************************
#METHODS
#*******************************************************

#' Downloads MODIS' hdf files. Wrapper of the MODIS::getHdf
#' 
#' @param object A ModisDownloader object
#' @return A character vector with the paths to the downloaded files
#' @docType methods
#' @export 
setGeneric(name = "downloadHdfs", def = function(object){standardGeneric("downloadHdfs")})
setMethod(
  f = "downloadHdfs",
  signature = "ModisDownloader",
  definition = function(object){
    
    res <- .downloadHdfs(argumentSeparator = " ", requestedTiles = getRequestedTiles(object), requestedProducts = getRequestedProducts(object), timeWindowStart = getTimeWindowStart(object), timeWindowEnd = getTimeWindowEnd(object))  
    return(res)
    
  }
)

#' Downloads the MODIS MOD09Q1 corresponding to the amazon (Tiles H <- 10:13; V <- 8:10)
#' 
#' @param object A ModisDownloader object
#' @param region Region name. One of "amazon", "southamerica"
#' @return Vector of the paths to the downloaded files
#' @docType methods
#' @export 
setGeneric(name = "downloadRegionHdfs", def = function(object, region){standardGeneric("downloadRegionHdfs")})
setMethod(
  f = "downloadRegionHdfs",
  signature = "ModisDownloader",
  definition = function(object, region){
    
    res <- NA
    fromDate <- getTimeWindowStart(object)
    toDate <- getTimeWindowEnd(object)
    if(length(region) == 1){
      if(region == "amazon"){
        res <- .downloadAmazonHdfs(fromDate, toDate)    
      }
      if(region == "southamerica"){
        res <- .downloadSouthamericaHdfs(fromDate, toDate)
      }
    }else{
      cat("Invalid number of arguments")
    }
    return (unlist(res))
  }
)

#' Finds invalid HDF files
#'
#' @param hdfFiles Character vector of HDF file paths
#' @return A list with the paths of invalid files
#' @docType methods
#' @export 
setGeneric(name = "findInvalidHdfs", def = function(object, hdfFiles){standardGeneric("findInvalidHdfs")})
setMethod(
  f = "findInvalidHdfs",
  signature = "ModisDownloader",
  definition = function(object, hdfFiles){
    res <- .findInvalidHdfs(hdfFiles = hdfFiles)
    return(res)
  }
)


#*******************************************************
#WORKER
#*******************************************************

# Finds invalid HDF files
#
# @param hdfFiles Character vector of HDF file paths
# @return A list with the paths of invalid files
.findInvalidHdfs <- function(hdfFiles){
  counter <- 0
  res <- list()
  for(hdf in hdfFiles){
    counter <- counter + 1
    cmd <- paste("gdalinfo ", hdf, sep = "")
    sysres <- system(cmd, intern = TRUE)#, ignore.stdout = TRUE, ignore.stderr = TRUE)
    
    if(length(sysres) == 0){
      if(attr(sysres,"status") == 1){
        cat ("Invalid HDF file")
        res[[counter]] <- hdf
        
      }
    }
  }
  return(res)
}  

# Downloads MODIS MOD09Q1 files
#
# @param argumentSeparator Argument separator in a string
# @param requestedTiles Tiles to download (i.e h19v11)
# @return Vector of the paths to the downloaded files
.downloadHdfs <- function(argumentSeparator, requestedTiles, requestedProducts, timeWindowStart, timeWindowEnd){
  
  wait = 1
  col <- "005"
  
  res <- list()  
  prods <- unlist(strsplit(requestedProducts, split = argumentSeparator))
  tiles <- unlist(strsplit(requestedTiles, split = argumentSeparator))
  hvList <- list()
  for(i in 1:(length(tiles))){
    tile <- tiles[i]
    hTile <- as.numeric(substr(tile, 2, 3))
    vTile <- as.numeric(substr(tile, 5, 6))
    hvList[[i]] <- list(hTile, vTile)
  }
  
  for(i in 1:(length(prods))){
    prod <- prods[i]
    res[[i]] <- mclapply(hvList, .dummy_modisDownloader, prod = prod, fromDate = timeWindowStart, toDate = timeWindowEnd, col = col, waitTime = wait)  
  }
  res <- unlist(res)
  return(res)
}


# Downloads the MODIS MOD09Q1 corresponding mainly to continental Southamerica
#
# @param fromDate Start date
# @param toDate End date
# @return Vector of the paths to the downloaded files
.downloadSouthamericaHdfs <- function(fromDate, toDate){
  prod <- "MOD09Q1"
  col <- "005"
  wait = 1
  
  tH1 <- 10:11
  tV1 <-7:10
  hv1 <- list(tH1, tV1)
  tH2 <- 12:13
  tV2 <- 9:13
  hv2 <- list(tH2, tV2)
  tH3 <- 13:14
  tV3 <- 14:14
  hv3 <- list(tH3, tV3)
  tH4 <- 14:14
  tV4 <- 9:11
  hv4 <- list(tH4, tV4)
  tH5 <- 11:11
  tV5 <- 11:12
  hv5 <- list(tH5, tV5)
  tH6 <- 9
  tV6 <- 9
  hv6 <- list(tH6, tV6)
  tH7 <- 12
  tV7 <- 8
  hv7 <- list(tH7, tV7)
  l <- list(hv1, hv2, hv3, hv4, hv5, hv6, hv7)
  
  res <- mclapply(l, .dummy_modisDownloader, prod = prod, fromDate = fromDate, toDate = toDate, col = col, waitTime = wait)
  return (res)
}



# Downloads the MODIS MOD09Q1 corresponding to the amazon
#
# @param fromDate Start date
# @param toDate End date
# @return Vector of the paths to the downloaded files
.downloadAmazonHdfs <- function(fromDate, toDate){
  prod <- "MOD09Q1"
  col <- "005"
  tH <- 10:13
  tV <-8:10
  wait = 1
  
  res <- .modisDownloader(prod, fromDate, toDate, tH, tV, col, wait)
  return (res)
}




#*******************************************************
#UTIL
#*******************************************************



# Dummy function for modisDownloader
#
# @param hvList List of list. Inner list contain the numeric vector with the horizontal and vertical tiles to download
# @param pro product parameter of MODIS::getHdf
# @param fromDate begin parameter of MODIS::getHdf
# @param toDate end parameter of MODIS::getHdf
# @param col collection parameter of MODIS::getHdf
# @param waitTime wait parameter of MODIS::getHdf
.dummy_modisDownloader <- function(hvList, prod, fromDate, toDate, col, waitTime){
  tH <- hvList[[1]]
  tV <- hvList[[2]]
  res <- .modisDownloader(prod = prod, fromDate = fromDate, toDate = toDate, tH = tH, tV = tV, col = col, waitTime = waitTime)
  return (res)
}

# Wrapper of the MODIS:getHdf function
#
# @param pro product parameter of MODIS::getHdf
# @param fromDate begin parameter of MODIS::getHdf
# @param toDate end parameter of MODIS::getHdf
# @param tH tileH parameter of MODIS::getHdf
# @param tV tileV parameter of MODIS::getHdf
# @param col collection parameter of MODIS::getHdf
# @param waitTime wait parameter of MODIS::getHdf
.modisDownloader <- function(prod, fromDate, toDate, tH, tV, col, waitTime){
  
  res <- getHdf(product = prod, 
                begin = fromDate, 
                end = toDate, 
                tileH = tH, 
                tileV = tV,
                collection = col, 
                wait = waitTime)  
  return (res)
}
