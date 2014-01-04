#' The UTIL class
#'
#' This class contains some utilitary functions
#'
#'
#' @note No notes
#' @name Util
#' @aliases Util-class
#' @exportClass Util
#' @author Alber Sanchez
setClass(
  Class = "Util", 
  slots = c(dummy = "character"),
  validity = function(object){
    #cat("~~~ Util: inspector ~~~ \n")
    res <- TRUE
    if(res == FALSE)
      stop ("[Util: validation] Some parameters are invalid")
    return(res)
  }
)

#*******************************************************
#CONSTRUCTOR
#*******************************************************
setMethod(
  f="initialize",
  signature="Util",
  definition=function(.Object, dummy = ""){
    #cat ("~~~~~ Util: initializator ~~~~~ \n")
    .Object@dummy <- dummy
    validObject(.Object)# call of the inspector
    return(.Object)
  }
)
#*******************************************************
#ACCESSORS
#*******************************************************

#*******************************************************
#GENERIC METHODS
#*******************************************************

#*******************************************************
#METHODS
#*******************************************************


#' Get the characters of a string from right to left - taken from http://stackoverflow.com/questions/7963898/extracting-the-last-n-characters-from-a-string-in-r
#' 
#' @param object An instance of the class Util
#' @param x A string
#' @param n The number of chars
#' @return The requested characters
#' @export
setGeneric(name = "substrRight", def = function(object, x, n){standardGeneric("substrRight")})
setMethod(
  f = "substrRight",
  signature = "Util",
  definition = function(object, x, n){
    
    res <- .substrRight(x = x, n = n)
    return(res)
    
  }
)


#' Find the extension of a file
#'
#' @param object An instance of the class Util
#' @param fileName Name of the file
#' @return The file extension
#' @export
setGeneric(name = "getFileExtension", def = function(object, fileName){standardGeneric("getFileExtension")})
setMethod(
  f = "getFileExtension",
  signature = "Util",
  definition = function(object, fileName){
    
    res <- .getFileExtension(fileName = fileName)
    return(res)
    
  }
)

#' Returns the filename of the path to the file
#'
#' @param object An instance of the class Util
#' @param filepath Character representing the full path to the file
#' @return Character representing the filename including the file extension
#' @export
setGeneric(name = "getFilenameFromFilepath", def = function(object, filepath){standardGeneric("getFilenameFromFilepath")})
setMethod(
  f = "getFilenameFromFilepath",
  signature = "Util",
  definition = function(object, filepath){
    
    res <- .getFilenameFromFilepath(filepath = filepath)
    return(res)
    
  }
)


#' Formats time
#'
#' @param object An instance of the class Util
#' @param time Character representing a date
#' @return A numeric representing a date (i.e 20120228)
#' @export
setGeneric(name = "processTime", def = function(object, time){standardGeneric("processTime")})
setMethod(
  f = "processTime",
  signature = "Util",
  definition = function(object, time){
    
    res <- .processTime(time = time)
    return(res)
    
  }
)


#' Gets the adquisition time of a MODIS HDF file name
#'
#' @param object An instance of the class Util
#' @param hdfFilename HDF filename
#' @return Character. A date in the format year and day of the year YYYYDOY
#' @export
setGeneric(name = "getTimeFromHdfFilename", def = function(object, hdfFilename){standardGeneric("getTimeFromHdfFilename")})
setMethod(
  f = "getTimeFromHdfFilename",
  signature = "Util",
  definition = function(object, hdfFilename){
    
    res <- .getTimeFromHdfFilename(hdfFilename = hdfFilename)
    return(res)
    
  }
)


#' Builds a file name for storing the results of processing
#'
#' @param object An instance of the class Util
#' @param fileName Character representing the file name
#' @param band Character representing the band
#' @param ext Character representing the extension for the results file (i.e ".txt")
#' @return Character representing a filename 
#' @export
setGeneric(name = "getFileresultFromFilename", def = function(object, fileName, band, ext){standardGeneric("getFileresultFromFilename")})
setMethod(
  f = "getFileresultFromFilename",
  signature = "Util",
  definition = function(object, fileName, band, ext){
    
    res <- .getFileresultFromFilename(fileName = fileName, band = band, ext = ext)
    return(res)
    
  }
)

#' Returns the filename without extension
#'
#' @param object An instance of the class Util
#' @param filename Character representing the file name including extension
#' @return Character representing the filename without the file extension
#' @export
setGeneric(name = "getFileNoExtension", def = function(object, filename){standardGeneric("getFileNoExtension")})
setMethod(
  f = "getFileNoExtension",
  signature = "Util",
  definition = function(object, filename){
    
    res <- .getFileNoExtension(filename = filename)
    return(res)
  }
)


#' Get ths MODIS tile id from the modis filename
#'
#' @param object An instance of the class Util
#' @param fileName Name of the file
#' @return The name of the file
#' @export
setGeneric(name = "getTileIdFromFilename", def = function(object, fileName){standardGeneric("getTileIdFromFilename")})
setMethod(
  f = "getTileIdFromFilename",
  signature = "Util",
  definition = function(object, fileName){
    
    res <- .getTileIdFromFilename(fileName = fileName)
    return(res)
  }
)


#' Returns the GMPI of the first pixel (top left) of the given MODIS tile
#'
#' @param object An instance of the class Util
#' @param modisTileId A character with a MODIS tile id (i.e "h10v08")
#' @param nrows Number of rows in a MODIS image (i.e for MOD09Q1 is 4800)
#' @param ncols Number of rows in a MODIS image (i.e for MOD09Q1 is 4800)
#' @return Numeric vector containing the c(i,j) pixel coordinates in th GMPI
#' @export
setGeneric(name = "getFirstGmip", def = function(object, modisTileId, nrows, ncols){standardGeneric("getFirstGmip")})
setMethod(
  f = "getFirstGmip",
  signature = "Util",
  definition = function(object, modisTileId, nrows, ncols){
    
    res <- .getFirstGmip(modisTileId = modisTileId, nrows = nrows, ncols = ncols)
    return(res)
  }
)


#' Returns the filepath of the path witout the last part (filename)
#'
#' @param object An instance of the class Util
#' @param filepath Character representing the full path to the file
#' @return Character representing the filepath without the file name
#' @export
setGeneric(name = "getFilepathFromFilepath", def = function(object, filepath){standardGeneric("getFilepathFromFilepath")})
setMethod(
  f = "getFilepathFromFilepath",
  signature = "Util",
  definition = function(object, filepath){
    
    res <- .getFilepathFromFilepath(filepath = filepath)
    return(res)
  }
)

#*******************************************************
#WORKER
#*******************************************************

# Returns the filepath of the path witout the last part (filename)
#
# @param filepath Character representing the full path to the file
# @return Character representing the filepath without the file name
.getFilepathFromFilepath <- function(filepath){
  filePathParts <- unlist(strsplit(filepath, split = "/"))
  res <- filePathParts[-length(filePathParts)]
  res <- paste0(res, sep = '/', collapse="")
  res <- substr(res, 1, nchar(res) - 1)
  return(res)
}




# Returns the GMPI of the first pixel (top left) of the given MODIS tile
#
# @param modisTileId A character with a MODIS tile id (i.e "h10v08")
# @param nrows Number of rows in a MODIS image (i.e for MOD09Q1 is 4800)
# @param ncols Number of rows in a MODIS image (i.e for MOD09Q1 is 4800)
# @return Numeric vector containing the c(i,j) pixel coordinates in th GMPI
.getFirstGmip <- function(modisTileId, nrows, ncols){
  thtv <- as.numeric(.getHV(modisTileId))
  iGpid <- thtv[1] * nrows
  jGpid <- thtv[2] * ncols
  res <- c(iGpid, jGpid)
}


# Get ths MODIS tile id from the modis filename
#
# @param fileName Name of the file
# @return The name of the file
.getTileIdFromFilename <- function(fileName){
  tmp <- unlist(strsplit(fileName, split = "[.]"))
  res <- tmp[3]
  return(res)
}


# Returns the filename without extension
#
# @param filename Character representing the file name including extension
# @return Character representing the filename without the file extension
.getFileNoExtension <- function(filename){
  fileNameParts <- unlist(strsplit(filename, split = "[.]"))
  noext <- fileNameParts[-length(fileNameParts)]
  res <- paste(noext, collapse = '.')
  return(res)
}


# Builds a file name for storing the results of processing
#
# @param fileName Character representing the file name
# @param band Character representing the band
# @param ext Character representing the extension for the results file (i.e ".txt")
# @return Character representing a filename 
.getFileresultFromFilename <- function(fileName, band, ext){
  fileNameNoExt <- .getFileNoExtension(fileName)
  fileNameNoExt <- gsub(pattern="-", replacement="_", x = fileNameNoExt)#SciDB does not suport "-" in array names
  res <- paste(fileNameNoExt, "band", band, ext, sep = "")
  return(res)
}



# Gets the adquisition time of a MODIS HDF file name
#
# @param hdfFilename HDF filename
# @return Character. A date in the format year and day of the year YYYYDOY
.getTimeFromHdfFilename <- function(hdfFilename){
  fileNameParts <- unlist(strsplit(hdfFilename, split = "[.]"))
  res <- substr(fileNameParts[2], 2, nchar(fileNameParts[2]))
  return (res)
}


# Formats time
#
# @param time Character representing a date
# @return A numeric representing a date (i.e 20120228)
.processTime <- function(time){
  res <- time
  
  if(nchar(time) == 7){# YYYYDOY
    tmp <- .ydoy2yearmonthday(time)
  }else{
    tmp <- as.POSIXlt(time)
  }
  res <- paste(format(tmp, "%Y"), format(tmp, "%m"), format(tmp, "%d"), sep = "")
  res <- as.numeric(res)
  return(res)
}


# Returns the filename of the path to the file
#
# @param filepath Character representing the full path to the file
# @return Character representing the filename including the file extension
.getFilenameFromFilepath <- function(filepath){
  filePathParts <- unlist(strsplit(filepath, split = "/"))
  res <- filePathParts[length(filePathParts)]
  return(res)
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

# Find the extension of a file
#
# @ param fileName Name of the file
# @ return The file extension
.getFileExtension <- function(fileName){
  res <- ""
  filenameParts <- unlist(strsplit(fileName, "[.]"))
  res <- filenameParts[length(filenameParts)]
  return(res)
}


# Finds if the given year is a leap year
#
# @param year NUmeric year
# @return TRUE is the year is leap, FALSE otherwise
.isLeapYear <- function(year){
  res <- FALSE
  d400 <- year %% 400 == 0
  d4 <- year %% 4 == 0
  d100 <- year %% 100 == 0
  if(d400 || d4){
    if(d100){
      res <- FALSE
    }else{
      res <- TRUE
    }
  }
  return(res)
}


# Transforms a date in the year-day_of_the_year format to a date
#
# @param YYYYDOY Character with 4 digits for the year and 3 for the day of the year (i.e 2012324)
# @return A date object
.ydoy2yearmonthday <- function(YYYYDOY){
  #http://disc.gsfc.nasa.gov/julian_calendar.shtml
  year <- as.numeric(substr(YYYYDOY, 1, 4))
  doy <- as.numeric(substr(YYYYDOY, 5, 7))
  res <- ""
  if(doy > 0 && doy < 367){
    firstdayRegular <- c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335, 366)
    firstdayLeap <- c(1, 32, 61, 92, 122, 153, 183, 214, 245, 275, 306, 336, 367)
    if(.isLeapYear(year)){
      firstday <- firstdayLeap
    }else{
      firstday <- firstdayRegular
    }
    for (i in 1:(length(firstday) - 1)){
      start <- firstday[i]
      end <- firstday[i + 1]
      if(doy >= start && doy < end){
        month <- i
        break
      }
    }
    day <- doy - firstday[month] + 1
    res <- as.POSIXlt(paste(year, month, day, sep = "/"))
  }
  return (res)
}


# Returns the tileH and tileV from a MODIS tile Id
#
# @param modisTileId A character with a MODIS tile id (i.e "h10v08")
# @return A character vector of 2 elements c(tH, tV)
.getHV <- function(modisTileId){
  tH <- substr(modisTileId, 2, 3)
  tV <- substr(modisTileId, 5, 6)
  res <- c(tH, tV)
  return(res)
} 