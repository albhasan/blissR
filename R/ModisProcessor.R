#' The MODISPROCESSOR class
#'
#' Use this class for processing downloaded MODIS tiles in order to produce files formated for uploading to SciDB.
#'
#' You can ???????
#'
#'@section Slots: 
#'  \describe{
#'    \item{\code{localArcPath}:}{Object of class \code{"character"}, it is the path to the folder where the MODIS files are stored. It is the path to the "MODIS_ARC" folder. See the MODIS package documentation - MODISoptions}
#'  }
#'
#' @note No notes
#' @name ModisProcessor
#' @rdname ModisProcessor
#' @aliases ModisProcessor-class
#' @exportClass ModisProcessor
#' @author Alber Sanchez
setClass(
  Class = "ModisProcessor", 
  slots = c(hdfFiles = "character", 
            resultFolder = "character"),
  validity = function(object){
    cat("~~~ ModisProcessor: inspector ~~~ \n")
    res <- TRUE
    if(nchar(object@hdfFiles) < 2)
      res <- FALSE
    if(nchar(object@resultFolder) < 2)
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
  definition=function(.Object, hdfFiles, resultFolder){
    cat ("~~~~~ ModisProcessor: initializator ~~~~~ \n")
    .Object@hdfFiles <- hdfFiles
    .Object@resultFolder <- resultFolder
    validObject(.Object)# call of the inspector
    return(.Object)
  }
)

#*******************************************************
#ACCESSORS
#*******************************************************
setGeneric("getHdfFiles",function(object){standardGeneric ("getHdfFiles")})
setMethod("getHdfFiles","ModisProcessor",
          function(object){
            return(object@hdfFiles)
          }
)

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
#' @return A boolean indicating success or failure
#' @docType methods
#' @rdname process-methods
#' @export 
setGeneric(name = "process", def = function(object){standardGeneric("processHdfs")})
setMethod(
  f = "process",
  signature = "ModisProcessor",
  definition = function(object){
    
    res <- .process(getHdfFiles(object), getResultFolder(object))  
    return(res)
    
  }
)


#*******************************************************
#WORKER
#*******************************************************

# Shadow function for process
#
# @ param files Character vector with the path to each file
# @ param resultFolder Path to the folder for storing the resulting files

.process <- function(files, resultFolder){
  mclapply(files, .processFile, resultFolder) 
}


# Process a single file according to its type
#
# @ param files Character vector with the path to each MODIS file
# @ param resultFolder Path to the folder for storing the resulting files
.processFile <- function(file, resultFolder){
  res <- ""
  fileExt <- .getFileExtension(file)
  if(fileExt == "hdf"){
    res <- .processHdf(filePath = file, resultFolder = resultFolder)
  }else if(fileExt == "nc"){
    res <- .processNcdf(filePath = file, resultFolder = resultFolder) 
  }
  return(res)
}




# Process a HDF file. HDF-EOS files can contain several bands
#
# @param hdfFilePath Path to the HDF file
# @param resultFolder Path to the folder where to store the resulting files
.processHdf <- function(filePath, resultFolder){
  
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
    resultFiles[i] <- .getFileresultFromFilename(fileName = fileName, band = bandName, ".txt")
  }
  res <- mclapply(bands, .dummy_processXXX, bandPaths = bandPaths, fileName = fileName, resultFiles= resultFiles, bandTimes = bandTimes)
  return (res)
}

# Process a NetCDF file. NetCDF files can contain several bands
#
# @param cdfFilePath Path to the HDF file
# @param resultFolder Path to the folder where to store the resulting files
.processNcdf <- function(filePath, resultFolder){
  ncdf <- raster(filePath)
  bands <- c(1:nbands(ncdf))
  fileName <- .getFilenameFromFilepath(filePath)
  bandPaths <- paste(filePath, bands, sep = "/")
  resultFiles <- vector(mode = "character", length = length(bands))
  bandTimes <- vector(mode = "numeric", length = length(bands))
  for(i in bands){
    resultFiles[i] <- .getFileresultFromFilename(fileName = fileName, band = i, ".txt")
    band <- raster(filePath, band = i)
    bandTimes[i] <- .processTime(slot(band,"z")[[1]])
  }
  res <- mclapply(bands, .dummy_processXXX, bandPaths = bandPaths, fileName = fileName, resultFiles= resultFiles, bandTimes = bandTimes)
  return (res)
}


#Process a single band of a file
#
# @param bandPath For HDFs, this is the imageSds of a specific band in the HDF file (Use MODIS::getSds). For NetCDFs this is the path to the file plus the band number (i.e netcdffile.nc/1)
# @param fileName Name of the file. i.e MOD09Q1.A2013281.h11v09.005.2013303130737.hdf
# @param resultFile Name of the result file
# @param tmpFilename OPTIONAL name of a temporal file
# @param bandTime Time when the image/band was taken
.processBand <- function(bandPath, fileName, resultFile, bandTime, tmpFilename = '') {
  
  #Deal with image formats
  fileExt <- .getFileExtension(fileName)
  if(fileExt  == "hdf"){# HDF file
    inputRaster <- raster(bandPath) 
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
  ncols <- dim(inputRaster)[2]
  imgExtent <- extent(inputRaster)
  imgResolution <- res(inputRaster)
  if (todisk) {#Chunk processing -> Writes a temporal file on disk
    for (i in 1:bs$n) {
      rowIndex <- bs$row[i]
      rowAmount <- bs$nrows[i]
      v <- getValues(inputRaster, row = bs$row[i], nrows = bs$nrows[i] )#Reads several image rows values as a vector
      tmp <- .getStuffTogether(v, row = rowIndex, nrows = rowAmount, ncols = ncols, imgExtent = imgExtent, imgResolution = imgResolution, time = bandTime)
      if(i == 1){
        write.table(tmp, file = resultFile, append = TRUE, sep = ",", row.names = FALSE, col.names = TRUE, quote = FALSE)
      }else{
        write.table(tmp, file = resultFile, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE, quote = FALSE)  
      }
    }
  } else {#Read all the raster into memory
    for (i in 1:bs$n) {
      rowIndex <- bs$row[i]
      rowAmount <- bs$nrows[i]
      v <- getValues(inputRaster, row = rowIndex, nrows = rowAmount)
      tmp <- .getStuffTogether(v, row = rowIndex, nrows = rowAmount, ncols = ncols, imgExtent = imgExtent, imgResolution = imgResolution, time = bandTime)
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
# @param row Index of the first row of image data contained in v
# @param nrows Number of rows which data is contained in v
# @param ncols Number of columns in the image
# @param imgExtent Extent object (raster package) of the image
# @param imgResolution Vector with the image resolution in x and y axis
# @param time Numeric. Time when the image/band was taken (i.e 20120304)
# @return A numeric matrix where each row is  pixel with columns x, y, time (Acquisition date YYYYDDD), pixel value
.getStuffTogether <- function(v, row, nrows, ncols, imgExtent, imgResolution, time){
  imgTimeV <- rep(time, times = (nrows * ncols))
  
  xRes <- imgResolution[1]
  yRes <- imgResolution[2]
  xstart = xmin(imgExtent) + (0.5 * xRes)
  ystart = (ymax(imgExtent) - (0.5 * yRes)) - (yRes * (row - 1))
  xCoordsV <- rep(seq(from = xstart, by = xRes, length.out = ncols), times = nrows)
  yCoordsV <- rep(seq(from = ystart, by = -yRes, length.out = nrows), each = ncols)
  
  tmpNumCols <- 4
  resNum <- vector(mode = "numeric", length = (nrows * tmpNumCols))
  resNum <- append(xCoordsV, yCoordsV)
  resNum <- append(resNum, imgTimeV)
  resNum <- append(resNum, v)
  res <- matrix(data = resNum, ncol = tmpNumCols, byrow = FALSE)
  colnames(res) <- c("xCoord", "yCoord", "tCoord", "value")
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
# @param bandTimes Vector numeric with the data when the images/bands were taken
.dummy_processXXX <- function(n, bandPaths, fileName, resultFiles, bandTimes){
  res <- .processBand(bandPath = bandPaths[n], fileName = fileName, resultFile = resultFiles[n], bandTime = bandTimes[n], tmpFilename = '') 
  return (res)
}