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
#'    \item{\code{baseUrl}:}{Object of class \code{"character"}, it is the URL for seaarching the MODIS tiles. For dowloading the tiles the MODIS package is used}
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
            baseUrl = "character"),
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
  definition=function(.Object, timeWindowStart, timeWindowEnd, requestedTiles, requestedProducts, collections, baseUrl){
    cat ("~~~~~ ModisDownloader: initializator ~~~~~ \n")
    .Object@timeWindowStart <- timeWindowStart
    .Object@timeWindowEnd <- timeWindowEnd
    .Object@requestedTiles <- requestedTiles
    .Object@requestedProducts <- requestedProducts
    .Object@collections <- collections
    .Object@baseUrl <- baseUrl
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


#*******************************************************
#GENERIC METHODS
#*******************************************************

#*******************************************************
#METHODS
#*******************************************************

#' Downloads MODIS' hdf files
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
    
    res <- .downloadHdfs(argumentSeparator = " ", modisCollections = getCollections(object), requestedTiles = getRequestedTiles(object), requestedProducts = getRequestedProducts(object), timeWindowStart = getTimeWindowStart(object), timeWindowEnd = getTimeWindowEnd(object), baseUrl = getBaseUrl(object))  
    return(res)
  
  }
)


#*******************************************************
#WORKER
#*******************************************************


# Dowloads HDF files from NASA's website
#
# @param argumentSeparator A character used for argument separation in a string
# @param modisCollections A character with the MODIS collection names to download. The elements are separated using argumentSeparator
# @param requestedTiles A character with the requested MODIS's tiles to download. The elements are separated using argumentSeparator
# @param requestedProducts A character with the MODIS' products to download. The elements are separated using argumentSeparator
# @ param timeWindowStart A character with the time window to filter data.
# @ param timeWindowEnd A character with the time window to filter data.
# @ param baseUrl The URL to look for the files
.downloadHdfs <- function(argumentSeparator, modisCollections, requestedTiles, requestedProducts, timeWindowStart, timeWindowEnd, baseUrl){

  #*******************************
  #PRELIMINARS
  #*******************************
  
  #Gets the arguments from single strings
  cat("Getting parameters...")
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
  cat("Building URLs...")
  dataFoldersColl <- paste(baseUrl, coll, sep = "/")# Adds the collections to the base URL
  dataFoldersProd <- unlist(lapply(dataFoldersColl, paste, products, sep = "/", collapse = NULL))# Adds the products to the URLs
  dataFoldersYear <- unlist(lapply(dataFoldersProd, paste, yearSeq, sep = "/", collapse = NULL))# Adds the years to the URLs
  #Checks if the folders exist in the server
  cat("Inspecting annual folders...")
  if(isWindows){
    foldersExist <- unlist(lapply(dataFoldersYear, url.exists))  
  }else{
    foldersExist <- unlist(mclapply(dataFoldersYear, url.exists))  
  }
  dataFoldersYear <- dataFoldersYear[foldersExist]# Selects only the existing folders in the server
  dataFoldersYear <- paste0(dataFoldersYear, "/", collapse = NULL)# curl needs and ending /
  # Gets the folder's contents  
  cat("Getting annual folder contents...")
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
    cat("Getting daily folder contents...")
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
      hdfDownloaded <- unlist(lapply(filteredTiles, .dummy.downloadHdf, wait = 0))  
    }else{
      hdfDownloaded <- unlist(mclapply(filteredTiles, .dummy.downloadHdf, wait = 0))
    }
    cat("Files downloaded.")
    #|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
  }else{
    cat("No HDF file meet the given parameters")
  }
  return(hdfDownloaded)
}




#*******************************************************
#UTIL
#*******************************************************


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