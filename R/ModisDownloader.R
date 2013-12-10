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
#'    \item{\code{baseUrl}:}{Object of class \code{"character"}, it is the URL for searching the MODIS tiles. For dowloading the tiles the MODIS package is used}
#'    \item{\code{localArcPath}:}{Object of class \code{"character"}, it is the path to the folder for storing the MODIS files. It is the path to the "MODIS_ARC" (usually it contains a folder called "PROCESSED"). See the MODIS package documentation - MODISoptions}
#'  }
#'
#' @note No notes
#' @name ModisDownloader 
#' @rdname ModisDownloader
#' @aliases ModisDownloader-class
#' @exportClass ModisDownloader
#' @author Alber Sanchez
setClass(
  Class = "ModisDownloader", 
  slots = c(timeWindowStart = "character", 
            timeWindowEnd = "character",
            requestedTiles = "character",
            requestedProducts = "character", 
            collections = "character", 
            baseUrl = "character", 
            localArcPath = "character"),
  validity = function(object){
    cat("~~~ ModisDownloader: inspector ~~~ \n")
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
    if(nchar(object@baseUrl) < 1)
      res <- FALSE
    if(nchar(object@localArcPath) < 1)
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
  definition=function(.Object, timeWindowStart, timeWindowEnd, requestedTiles, requestedProducts, collections, baseUrl, localArcPath){
    cat ("~~~~~ ModisDownloader: initializator ~~~~~ \n")
    .Object@timeWindowStart <- timeWindowStart
    .Object@timeWindowEnd <- timeWindowEnd
    .Object@requestedTiles <- requestedTiles
    .Object@requestedProducts <- requestedProducts
    .Object@collections <- collections
    .Object@baseUrl <- baseUrl
    .Object@localArcPath <- localArcPath
    validObject(.Object)# call of the inspector
    return(.Object)
  }
)

#*******************************************************
#ACCESSORS
#*******************************************************
setGeneric("getTimeWindowStart",function(object){standardGeneric ("getTimeWindowStart")})
setMethod("getTimeWindowStart","ModisDownloader",
          function(object){
            return(object@timeWindowStart)
          }
)
setGeneric("getTimeWindowEnd",function(object){standardGeneric ("getTimeWindowEnd")})
setMethod("getTimeWindowEnd","ModisDownloader",
          function(object){
            return(object@timeWindowEnd)
          }
)
setGeneric("getRequestedTiles",function(object){standardGeneric ("getRequestedTiles")})
setMethod("getRequestedTiles","ModisDownloader",
          function(object){
            return(object@requestedTiles)
          }
)
setGeneric("getRequestedProducts",function(object){standardGeneric ("getRequestedProducts")})
setMethod("getRequestedProducts","ModisDownloader",
          function(object){
            return(object@requestedProducts)
          }
)
setGeneric("getCollections",function(object){standardGeneric ("getCollections")})
setMethod("getCollections","ModisDownloader",
          function(object){
            return(object@collections)
          }
)
setGeneric("getBaseUrl",function(object){standardGeneric ("getBaseUrl")})
setMethod("getBaseUrl","ModisDownloader",
          function(object){
            return(object@baseUrl)
          }
)          
setGeneric("getLocalArcPath",function(object){standardGeneric ("getLocalArcPath")})
setMethod("getLocalArcPath","ModisDownloader",
          function(object){
            return(object@localArcPath)
          }
)


#*******************************************************
#GENERIC METHODS
#*******************************************************

#*******************************************************
#METHODS
#*******************************************************

#' DEPRECATED: Downloads MODIS' hdf files
#' 
#' @return A character vector with the paths to the downloaded files
#' @docType methods
#' @rdname downloadHdfs-methods
#' @export 
setGeneric(name = "downloadHdfs", def = function(object){standardGeneric("downloadHdfs")})
setMethod(
  f = "downloadHdfs",
  signature = "ModisDownloader",
  definition = function(object){
    
    res <- .downloadHdfs(argumentSeparator = " ", modisCollections = getCollections(object), requestedTiles = getRequestedTiles(object), requestedProducts = getRequestedProducts(object), timeWindowStart = getTimeWindowStart(object), timeWindowEnd = getTimeWindowEnd(object), baseUrl = getBaseUrl(object), localArcPath = getLocalArcPath(object))  
    return(res)
    
  }
)

#' Downloads the MODIS MOD09Q1 corresponding to the amazon (Tiles H <- 10:13; V <- 8:10)
#' 
#' @param region Region name. One of "amazon", "southamerica"
#' @param fromDate Start date
#' @param toDate End date
#' @return Vector of the paths to the downloaded files
downloadRegionHdfs <- function(region, fromDate, toDate){
  res <- NA
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
  return (res)
}




#*******************************************************
#WORKER
#*******************************************************




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
  fromDate
  wait = 1
  
  res <- .modisDownloader(prod, fromDate, toDate, tH, tV, col, wait)
  return (res)
}

# Dowloads HDF files from NASA's website
#
# @param argumentSeparator A character used for argument separation in a string
# @param modisCollections A character with the MODIS collection names to download. The elements are separated using argumentSeparator
# @param requestedTiles A character with the requested MODIS's tiles to download. The elements are separated using argumentSeparator
# @param requestedProducts A character with the MODIS' products to download. The elements are separated using argumentSeparator
# @ param timeWindowStart A character with the time window to filter data.
# @ param timeWindowEnd A character with the time window to filter data.
# @ param baseUrl The URL to look for the files
# @ param localArcPath Path to the folder taht will contain the MODIS files (See modis package documentation)
.downloadHdfs <- function(argumentSeparator, modisCollections, requestedTiles, requestedProducts, timeWindowStart, timeWindowEnd, baseUrl, localArcPath){
  
  #*******************************
  #PRELIMINARS
  #*******************************
  require(MODIS)
  MODISoptions(localArcPath = localArcPath, outDirPath = paste0(localArcPath,"/PROCESSED", collapse = NULL), quiet = TRUE)  
  t0 <- Sys.time()
  waitParameter <- 1
  #Gets the arguments from single strings
  cat("Getting parameters...\n")
  coll <- .processStringArgument(modisCollections, argumentSeparator)
  tiles <- .processStringArgument(requestedTiles, argumentSeparator)
  products <- .processStringArgument(requestedProducts, argumentSeparator)
  
  #Builds the time interval
  start <- as.POSIXlt(timeWindowStart)
  end <- as.POSIXlt(timeWindowEnd)
  timeWindow <- c(start, end)
  #Builds a sequence of years
  yearSeq <- .getYearList(start, end)
  isWindows <- Sys.info()[['sysname']] == "Windows"
  
  #*******************************
  #Builds the URL of the products
  #*******************************
  cat("Building URLs...\n")
  dataFoldersColl <- paste(baseUrl, coll, sep = "/")# Adds the collections to the base URL
  dataFoldersProd <- unlist(lapply(dataFoldersColl, paste, products, sep = "/", collapse = NULL))# Adds the products to the URLs
  dataFoldersYear <- unlist(lapply(dataFoldersProd, paste, yearSeq, sep = "/", collapse = NULL))# Adds the years to the URLs
  #Checks if the folders exist in the server
  cat("Inspecting annual folders...\n")
  if(isWindows){
    foldersExist <- unlist(lapply(dataFoldersYear, url.exists))  
  }else{
    foldersExist <- unlist(mclapply(dataFoldersYear, url.exists))  
  }
  dataFoldersYear <- dataFoldersYear[foldersExist]# Selects only the existing folders in the server
  dataFoldersYear <- paste0(dataFoldersYear, "/", collapse = NULL)# curl needs and ending /
  # Gets the folder's contents  
  cat("Getting annual folder contents...\n")
  if(isWindows){
    ftpContents <- lapply(dataFoldersYear, getURL, dirlistonly = TRUE)
  }else{
    ftpContents <- mclapply(dataFoldersYear, getURL, dirlistonly = TRUE)
  }
  ftpContents <- lapply(ftpContents, strsplit, "\n")
  ftpContents <- lapply(ftpContents, unlist)# Days of the year on each year folder
  i <- seq(from = 1, to = length(dataFoldersYear))
  dataFoldersDay <- lapply(i, .dummy.buildUrls, dataFoldersYear, ftpContents)# Adds day-of-the-year to the URLs
  dataFoldersDay <- unlist(dataFoldersDay)
  dataFoldersDay <- paste0(dataFoldersDay, "/", collapse = NULL)
  folderTimeFilter <- unlist(lapply(dataFoldersDay, .dummy.testUrlDates, startDate = start, endDate = end))
  if(!all(folderTimeFilter == FALSE)){
    dataFoldersDayTimeFiltered <- dataFoldersDay[folderTimeFilter]# Folder filtering using dates
    #Get the HDF file names of each day-folder  
    cat("Getting daily folder contents...\n")
    if(isWindows){
      files <- unlist(lapply(dataFoldersDayTimeFiltered, getURL, dirlistonly = TRUE))
    }else{
      files <- unlist(mclapply(dataFoldersDayTimeFiltered, getURL, dirlistonly = TRUE))
    }
    files <- unlist(lapply(files, strsplit, "\n"))
    filterTiles <- unlist(lapply(files, .isFileMatchingTiles, tiles))
    filteredTiles <- files[filterTiles]# Filter the tiles by tile index
    #|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
    #Downloads the HDF files
    cat("Starting to download. Number of files: ", length(filteredTiles))
    if(isWindows){
      hdfDownloaded <- unlist(lapply(filteredTiles, .dummy.downloadHdf, wait = waitParameter))  
    }else{
      hdfDownloaded <- unlist(mclapply(filteredTiles, .dummy.downloadHdf, wait = waitParameter))
    }
    cat("Files downloaded.\n")
    #|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
  }else{
    cat("No HDF file meet the given parameters")
  }
  cat(paste("Duration: ", Sys.time() - t0))
  return(hdfDownloaded)
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
  tH <- hvList[1]
  tV <- hvList[2]
  res <- .modisDownloader <- function(prod, fromDate, toDate, tH, tV, col, waitTime)
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


# DUMMY FOR APPLY. Call the MODIS::getHdf function for downloading MODIS' HDF files.
#
# @param hdfFilename Name of the HDF file to download
# @param wait Waiting time between ftp calls. See MODIS::getHdf help
# @return A character vector with the paths to the downloaded files
.dummy.downloadHdf <- function(hdfFilename, wait){
  res <- getHdf(HdfName = hdfFilename, wait = wait) 
  return(res)
}


# DUMMY FOR APPLY. Tests if the date elements of the URL fall in the given time interval
#
# @param url URL of a MODIS folder
# @param url URL of a MODIS folder
# @param startDate Start date of the time interval
# @param endDate End date of the time interval
# @return TRUE is the URL's date falls in the interval, FALSE otherwise
.dummy.testUrlDates <- function(url, startDate, endDate){
  res <- FALSE
  timeInterval <- .getYearDayFromUrl(url)
  urlDate <- paste0(timeInterval[1], timeInterval[2])
  start <- paste0(format(startDate, "%Y"), strftime(startDate, format = "%j"))
  end <- paste0(format(endDate, "%Y"), strftime(endDate, format = "%j"))
  if(start <= urlDate && urlDate <= end)
    res <- TRUE
  return(res)
}


# Checks if the filename contains any of the given tiles
#
# @param hdfFilename Name of a HDF file
# @tiles A character vector of tiles for comparison
# @return TRUE if the tile reference contained in the file name matchs any of the given tiles
.isFileMatchingTiles <- function(hdfFilename, tiles){
  res <- FALSE
  filenameElements <- unlist(strsplit(hdfFilename, "[.]"))
  tile = filenameElements[3]
  if(is.element(tile, tiles)){
    res <- TRUE
  }
  return(res)
}


# URL builder helper function
#
# @param i A vector of integer indexes for the baseVector and the complementList
# @param baseVector A vector of base Urls
# @param complementList A list character vectors to append to each element of base vector
# @return a list of URLs
.dummy.buildUrls <- function(i, baseVector, complementList){
  res <- paste0(baseVector[i], complementList[[i]], collapse = NULL)
  return(res)
}


# Get the year and day-of-the-year from a URL
#
# @param url Example - "ftp://ladsweb.nascom.nasa.gov/allData/5/MYD09Q1/2012/361/
# @return a character vector (year, day-of-the-year)
.getYearDayFromUrl <- function(url){
  elements <- unlist(strsplit(url, split = "/")) 
  doty <- elements[length(elements)]
  year <- elements[length(elements) - 1]
  res <- c(year, doty)
  return(res)
}


# Builds a sequence of years from the start to the end dates
# 
# @param start The initial date of a time interval
# @param end The final date of a time interval
# @return a vector of years (numeric)
.getYearList <- function (start, end){
  res <- 0
  if(end$year - start$year < 1){
    res <- start$year + 1900
  }else{
    res <- seq(from = start$year + 1900, to = end$year + 1900, by = 1)
  }
  return (res)
}


# Get the characters of a string from right to left - taken from http://stackoverflow.com/questions/7963898/extracting-the-last-n-characters-from-a-string-in-r
# 
# @param x A string
# @param n The number of chars
# @return The requested characters
.substrRight <- function(x, n){
  res <- substr(x, nchar(x)-n+1, nchar(x))
  return(res)
}


#Get a single string as input and returns a list of arguments
#
# @param argument A string representing a list
# @param separator A character used as argument separator
# @return A character vector
.processStringArgument <- function(argument, separator){
  res <- unlist(strsplit(argument, separator))
  return(res)
}