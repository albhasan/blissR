#' The MODISPROCESSOR class
#'
#' Use this class for processing downloaded MODIS tiles in order to produce files formated for uploading to SciDB.
#'
#' You can ???????
#'
#'@section Slots: 
#'  \describe{
#'    \item{\code{files}:}{Object of class \code{"character"}, it is a vector with the paths to the HDF files.}
#'    \item{\code{resultFolder}:}{Object of class \code{"character"}, it is the path to the folder for storing results.}'    
#'    \item{\code{modisGrid}:}{Object of class \code{"modisGrid"}, it is a holder for MODIS grid properties.}'    
#'  }
#'
#' @note No notes
#' @name ModisProcessor
#' @aliases ModisProcessor-class
#' @exportClass ModisProcessor
#' @author Alber Sanchez
setClass(
  Class = "ModisProcessor", 
  slots = c(files = "character", 
            resultFolder = "character", 
            modisGrid = "ModisGrid"),
  validity = function(object){
    #cat("~~~ ModisProcessor: inspector ~~~ \n")
    res <- TRUE
    if(length(object@files) < 1)
      res <- FALSE
    if(nchar(object@resultFolder) < 2)
      res <- FALSE
    if(is.null(object@modisGrid))
      res <- FALSE
    if(res == FALSE)
      stop ("[ModisProcessor: validation] Some parameters are invalid")
    return(res)
  }
)

#*******************************************************
#CONSTRUCTOR
#*******************************************************
setMethod(
  f="initialize",
  signature="ModisProcessor",
  definition=function(.Object, files, resultFolder){
    #cat ("~~~~~ ModisProcessor: initializator ~~~~~ \n")
    .Object@files <- files
    .Object@resultFolder <- resultFolder
    .Object@modisGrid <- new("ModisGrid")
    validObject(.Object)# call of the inspector
    return(.Object)
  }
)

#*******************************************************
#ACCESSORS
#*******************************************************
# Returns the object files
# 
# @param object A ModisProcessor object
# @docType methods
# @export 
#setGeneric("getFiles",function(object){standardGeneric ("getFiles")})
setMethod("getFiles","ModisProcessor",
          function(object){
            return(object@files)
          }
)

#' Returns the object's result folder path
#' 
#' @param object A ModisProcessor object
#' @docType methods
#' @export 
setGeneric("getModisGrid",function(object){standardGeneric ("getModisGrid")})
setMethod("getModisGrid","ModisProcessor",
          function(object){
            return(object@modisGrid)
          }
)

#' Returns the object's MODIS grid
#' 
#' @param object A ModisProcessor object
#' @docType methods
#' @export 
setGeneric("getResultFolder",function(object){standardGeneric ("getResultFolder")})
setMethod("getResultFolder","ModisProcessor",
          function(object){
            return(object@resultFolder)
          }
)




#*******************************************************
#GENERIC METHODS
#*******************************************************

#*******************************************************
#METHODS
#*******************************************************

#' Process the input files
#' 
#' @param object A ModisProcessor object
#' @return A character vector with the paths to the result files
#' @docType methods
#' @export 
setGeneric(name = "process", def = function(object){standardGeneric("process")})
setMethod(
  f = "process",
  signature = "ModisProcessor",
  definition = function(object){
    # @param object A ModisProcessor object
    res <- .process(files = getFiles(object), resultFolder = getResultFolder(object), modisGrid = getModisGrid(object))
    return(res)
  }
)


#*******************************************************
#WORKER
#*******************************************************

# Shadow function for process
#
# @param files List with character vector with the path to each file
# @param resultFolder Path to the folder for storing the resulting files
# @param modisGrid A ModisGrid object
# @return A character vector with the paths to the result files
.process <- function(files, resultFolder, modisGrid){
  if(is.list(files)){
    f <- files
  }else{
    f <- as.list(files)
  }
  res <- mclapply(f, .processFile, resultFolder = resultFolder, modisGrid = modisGrid) 
  return (unlist(res))
}


# Process a single file according to its type
#
# @param filePath Character vector with the path to each MODIS file
# @param resultFolder Path to the folder for storing the resulting files
# @param modisGrid A ModisGrid object
# @return A character vector with the paths to the result files
.processFile <- function(filePath, resultFolder, modisGrid){
  res <- ""
  fileExt <- .getFileExtension(filePath)
  if(fileExt == "hdf"){
    res <- .processHdf(filePath = filePath, resultFolder = resultFolder, modisGrid = modisGrid)
  }else if(fileExt == "nc"){
    res <- .processNcdf(filePath = filePath, resultFolder = resultFolder, modisGrid = modisGrid)
  }
  return(res)
}


# Process a HDF file. HDF-EOS files can contain several bands
#
# @param hdfFilePath Path to the HDF file
# @param resultFolder Path to the folder where to store the resulting files
# @param modisGrid A ModisGrid object
# @return A character vector with the paths to the result files
.processHdf <- function(filePath, resultFolder, modisGrid){
  
  fileName <- .getFilenameFromFilepath(filePath)
  imageSds <- getSds(filePath)
  bands <- c(1:length(imageSds[[1]]))
  imgTime <- .processTime(.getTimeFromHdfFilename(fileName))
  bandTimes <- rep(imgTime, times = length(bands))
  bandPaths <- vector(mode = "character", length = length(bands))
  resultFiles <- vector(mode = "character", length = length(bands))
  for(i in bands){
    bp <- get("SDS4gdal", imageSds)[i]
    bandPaths[i] <- bp
    bandName <- paste(unlist(strsplit(bp, split = ":"))[c(4, 5)], collapse = "")
    resultFilename <- .getFileresultFromFilename(fileName = fileName, band = bandName, ext = ".txt")
    resultFiles[i] <- paste0(resultFolder, "/", resultFilename)
  }
  res <- mclapply(bands, .dummy_processXXX, 
                  bandPaths = bandPaths, 
                  fileName = fileName, 
                  resultFiles= resultFiles, 
                  bandTimes = bandTimes, 
                  modisGrid = modisGrid)
  return (unlist(res))
}

# Process a NetCDF file. NetCDF files can contain several bands
#
# @param cdfFilePath Path to the HDF file
# @param resultFolder Path to the folder where to store the resulting files
# @param modisGrid A ModisGrid object
# @return A character vector with the paths to the result files
.processNcdf <- function(filePath, resultFolder, modisGrid){
  ncdf <- raster(filePath)
  bands <- c(1:nbands(ncdf))
  fileName <- .getFilenameFromFilepath(filePath)
  bandPaths <- paste(filePath, bands, sep = "/")
  resultFiles <- vector(mode = "character", length = length(bands))
  bandTimes <- vector(mode = "numeric", length = length(bands))
  for(i in bands){
    resultFilename <- .getFileresultFromFilename(fileName = fileName, band = i, ext = ".txt")
    resultFiles[i] <- paste0(resultFolder, "/", resultFilename)
    band <- raster(filePath, band = i)
    bandTimes[i] <- .processTime(slot(band,"z")[[1]])
  }
  res <- mclapply(bands, .dummy_processXXX, bandPaths = bandPaths, fileName = fileName, resultFiles= resultFiles, bandTimes = bandTimes, modisGrid = modisGrid)
  return (unlist(res))
}


#Process a single band of a file
#
# @param bandPath For HDFs, this is the imageSds of a specific band in the HDF file (Use MODIS::getSds). For NetCDFs this is the path to the file plus the band number (i.e netcdffile.nc/1)
# @param fileName Name of the file. i.e MOD09Q1.A2013281.h11v09.005.2013303130737.hdf
# @param resultFile Name of the result file
# @param modisGrid A ModisGrid object
# @param tmpFilename OPTIONAL name of a temporal file
# @param bandTime Time when the image/band was taken
.processBand <- function(bandPath, fileName, resultFile, bandTime, modisGrid, tmpFilename = '') {
  
  
  
  
  #**************************************************************
  #TODO:
  #Make generic bi writing the table, no headers avoiding the if test for the first row
  #at the end, open the file and add the column names OOORRRRRR add the single dimmension header and foot of scidb format
  #**************************************************************
  
  
  #Deal with image formats
  fileExt <- .getFileExtension(fileName)
  if(fileExt  == "hdf"){# HDF file
    inputRaster <- raster(bandPath) 
    modisTileId <- .getTileIdFromFilename(fileName)
  }else if(fileExt  == "nc"){# NetCDF file
    tmpPathParts <- unlist(strsplit(bandPath, "/"))
    bandNumber <- tmpPathParts[length(tmpPathParts)]
    fname <- substr(bandPath, 1, (nchar(bandPath) - (nchar(bandNumber) + 1)))
    inputRaster <- raster(fname, band = as.integer(bandNumber)) 
  }
  #Chunk processing
  isBig <- ! canProcessInMemory(inputRaster, 3)#Number of raster copies to hold in memory
  tmpFilename <- trim(tmpFilename)
  if (isBig & tmpFilename == '') {
    tmpFilename <- rasterTmpFile()#Creates a temporal file to hold a big raster
  }
  if (tmpFilename != '') {
    todisk <- TRUE
  } else {
    todisk <- FALSE
  }
  bs <- blockSize(inputRaster)
  nrows <- dim(inputRaster)[1]
  ncols <- dim(inputRaster)[2]
  imgExtent <- extent(inputRaster)
  imgResolution <- res(inputRaster)
  
  for(i in 1:bs$n){
    startRow <- bs$row[i]
    nsubsetrows <- bs$nrows[i]
    v <- getValues(inputRaster, row = bs$row[i], nrows = bs$nrows[i] )#Reads several image rows values as a vector
    tmp <- .getStuffTogether(v = v, 
                             startRow = startRow, 
                             nsubsetrows = nsubsetrows, 
                             nrows = nrows, 
                             ncols = ncols, 
                             imgExtent = imgExtent, 
                             imgResolution = imgResolution, 
                             shootingDate = bandTime, 
                             modisGrid = modisGrid, 
                             modisTileId = modisTileId)
    if (todisk) {#Chunk processing -> Writes a temporal file on disk
      if(i == 1){
        write.table(tmp, file = resultFile, append = TRUE, sep = ",", row.names = FALSE, col.names = TRUE, quote = FALSE)
      }else{
        write.table(tmp, file = resultFile, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE, quote = FALSE)  
      }
    }else{# All  in memory
      if(i == 1){
        write.table(tmp, file = resultFile, append = TRUE, sep = ",", row.names = FALSE, col.names = TRUE, quote = FALSE)
      }else{
        write.table(tmp, file = resultFile, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE, quote = FALSE)  
      }
    }
  }
  
  return(resultFile)
}


#Creates an array out of set of rows of a raster
#
# @param v Vector with the image values
# @param startRow Index of the first row of image data contained in v
# @param nsubsetrows Number of consecutive rows taken from the image
# @param nrows Number of rows in the image
# @param ncols Number of columns in the image
# @param imgExtent Extent object (raster package) of the image
# @param imgResolution Vector with the image resolution in x and y axis
# @param shootingDate Numeric. Time when the image/band was taken (i.e 20120304)
# @param modisGrid A ModisGrid object
# @param modisTileId MODIS tile id (i.e h10v08)
# @return A numeric matrix where each row is  pixel with columns x, y, time (Acquisition date YYYYDDD), pixel value
.getStuffTogether <- function(v, startRow, nsubsetrows, nrows, ncols, imgExtent, imgResolution, shootingDate, modisGrid, modisTileId){
  imgTimeV <- rep(shootingDate, times = nsubsetrows * ncols)
  lxy <- calculateRowSubsetPixelCoords(modisGrid, nrows = nrows, ncols = ncols, nsubsetrows=  nsubsetrows, startRow = startRow)#calculateRowSubsetCoords(modisGrid)
  dxy <- displacePixelToGmpi(modisGrid, 
                             modisTileId = modisTileId, 
                             nrows = nrows, 
                             ncols = ncols, 
                             iLocalCoords = lxy[[1]], 
                             jLocalCoords = lxy[[2]])
  xCoordsV <- dxy[[1]]
  yCoordsV <- dxy[[2]]
  tmpNumCols <- 4
  resNum <- vector(mode = "numeric", length = nsubsetrows * ncols)
  resCoords <- append(xCoordsV, yCoordsV)
  resTvalues <- append(imgTimeV, v)
  resNum <- append(resCoords, resTvalues)
  res <- matrix(data = resNum, ncol = tmpNumCols, byrow = FALSE)
  colnames(res) <- c("i", "j", "t", "value")
  return (res)
}



#*******************************************************
#UTIL
#*******************************************************

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

# Returns the filename of the path to the file
#
# @param filepath Character representing the full path to the file
# @return Character representing the filename including the file extension
.getFilenameFromFilepath <- function(filepath){
  filePathParts <- unlist(strsplit(filepath, split = "/"))
  res <- filePathParts[length(filePathParts)]
  return(res)
}


# Returns the filename without extension
#
# @param filename Character representing the file name including extension
# @return Character representing the filename without the file extension
.getFileNoExtension <- function(filename){
  fileNameParts <- unlist(strsplit(filename, split = "[.]"))
  res <- fileNameParts[-length(fileNameParts)]
  res <- paste(res, sep = ".", collapse = '')
  return(res)
}

# Builds a file name for storing the results of processing
#
# @param filename Character representing the file name
# @param band Character representing the band
# @param ext Character representing the extension for the results file (i.e ".txt")
# @return Character representing a filename 
.getFileresultFromFilename <- function(fileName, band, ext){
  fileNameNoExt <- .getFileNoExtension(fileName)
  res <- paste(fileNameNoExt, "band", band, ext, sep = "")
  return(res)
}

# Gets the adquisition time of a MODIS HDF file name
#
# @param hdfFilename HDF filename
# @param Character. A date in the format year and day of the year YYYYDOY
.getTimeFromHdfFilename <- function(hdfFilename){
  fileNameParts <- unlist(strsplit(hdfFilename, split = "[.]"))
  res <- substr(fileNameParts[2], 2, nchar(fileNameParts[2]))
  return (res)
}

# Dummy function for LAPPLY in .processHDF or .processNcdf
#
# @param n Vector numeric with indexes to the input vectors
# @param bandPaths Vector character of the full paths to the bands inside the files
# @param fileName Name of the file (No path but include extension)
# @param resultFiles Vector character with full paths to the files for storing the results
# @param modisGrid A ModisGrid object
# @param bandTimes Vector numeric with the data when the images/bands were taken
# @return A character with the path to the result file
.dummy_processXXX <- function(n, bandPaths, fileName, resultFiles, bandTimes, modisGrid){
  res <- .processBand(bandPath = bandPaths[n], 
                      fileName = fileName, 
                      resultFile = resultFiles[n], 
                      bandTime = bandTimes[n], 
                      modisGrid = modisGrid, 
                      tmpFilename = '') 
  return (res)
}

# Get ths MODIS tile id from the modis filename
#
# @param fileName Name of the file
# @returns The name of the file
.getTileIdFromFilename <- function(fileName){
  tmp <- unlist(strsplit(fileName, split = "[.]"))
  res <- tmp[3]
  return(res)
}