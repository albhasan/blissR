#' The PROCESSOR class
#'
#' Use this class for processing data files in order to produce files formated for SciDB.
#'
#'
#'@section Slots: 
#'  \describe{
#'    \item{\code{files}:}{Object of class \code{"character"}, it is a vector with the paths to the input files.}
#'    \item{\code{resultFolder}:}{Object of class \code{"character"}, it is the path to the folder for storing the resulting files.}'    
#'  }
#'
#' @note No notes
#' @name Processor
#' @aliases Processor-class
#' @exportClass Processor
#' @author Alber Sanchez
setClass(
  Class = "Processor", 
  slots = c(files = "character", 
            resultFolder = "character"),
  validity = function(object){
    #cat("~~~ Processor: inspector ~~~ \n")
    res <- TRUE
    if(length(object@files) < 1)
      res <- FALSE
    if(nchar(object@resultFolder) < 2)
      res <- FALSE
    if(res == FALSE)
      stop ("[Processor: validation] Some parameters are invalid")
    return(res)
  }
)

#*******************************************************
#CONSTRUCTOR
#*******************************************************
setMethod(
  f="initialize",
  signature="Processor",
  definition=function(.Object, files, resultFolder){
    #cat ("~~~~~ Processor: initializator ~~~~~ \n")
    .Object@files <- files
    .Object@resultFolder <- resultFolder
    validObject(.Object)# call of the inspector
    return(.Object)
  }
)

#*******************************************************
#ACCESSORS
#*******************************************************

#' Returns the object files
#' 
#' @docType methods
#' @export 
setGeneric("getFiles",function(object){standardGeneric ("getFiles")})
setMethod("getFiles","Processor",
          function(object){
            # @param object A Processor object
            return(object@files)
          }
)


#' Returns the object's result folder
#' 
#' @param object A Processor object
#' @docType methods
#' @export 
setGeneric("getResultFolder",function(object){standardGeneric ("getResultFolder")})
setMethod("getResultFolder","Processor",
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
#' @param object A Processor object
#' @return A character vector with the paths to the result files
#' @docType methods
#' @export 
setGeneric(name = "process", def = function(object){standardGeneric("process")})
setMethod(
  f = "process",
  signature = "Processor",
  definition = function(object){
    res <- .process(files = object@files, resultFolder = getResultFolder(object))
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
# @return A character vector with the paths to the result files
.process <- function(files, resultFolder){
  if(is.list(files)){
    f <- files
  }else{
    f <- as.list(files)
  }
  res <- mclapply(f, .processFile, resultFolder = resultFolder) 
  return (unlist(res))
}


# Process a single file according to its type
#
# @param filePath Character vector with the path to each input file
# @param resultFolder Path to the folder for storing the resulting files
# @return A character vector with the paths to the result files
.processFile <- function(filePath, resultFolder){
  res <- ""
  u <- new("Util")
  fileExt <- getFileExtension(u, filePath)
  if(fileExt == "hdf"){
    res <- .processHdf(filePath = filePath, resultFolder = resultFolder)
  }else if(fileExt == "nc"){
    res <- .processNcdf(filePath = filePath, resultFolder = resultFolder)
  }
  return(res)
}


# Process a HDF file. HDF-EOS files can contain several bands
#
# @param hdfFilePath Path to the HDF file
# @param resultFolder Path to the folder where to store the resulting files
# @return A character vector with the paths to the result files
.processHdf <- function(filePath, resultFolder){
  
  u <- new("Util")
  fileName <- getFilenameFromFilepath(u, filePath)
  imageSds <- getSds(filePath)
  bands <- c(1:length(imageSds[[1]]))
  imgTime <- processTime(u, getTimeFromHdfFilename(u, fileName))
  bandTimes <- rep(imgTime, times = length(bands))
  bandPaths <- vector(mode = "character", length = length(bands))
  resultFiles <- vector(mode = "character", length = length(bands))
  for(i in bands){
    bp <- get("SDS4gdal", imageSds)[i]
    bandPaths[i] <- bp
    bandName <- paste(unlist(strsplit(bp, split = ":"))[c(4, 5)], collapse = "")
    resultFilename <- getFileresultFromFilename(u, fileName = fileName, band = bandName, ext = ".txt")
    resultFiles[i] <- paste0(resultFolder, "/", resultFilename)
  }
  res <- mclapply(bands, .dummy_processXXX, 
                  bandPaths = bandPaths, 
                  fileName = fileName, 
                  resultFiles= resultFiles, 
                  bandTimes = bandTimes)
  return (unlist(res))
}

#Process a single band of a file
#
# @param bandPath For HDFs, this is the imageSds of a specific band in the HDF file (Use MODIS::getSds). For NetCDFs this is the path to the file plus the band number (i.e netcdffile.nc/1)
# @param fileName Name of the file. i.e MOD09Q1.A2013281.h11v09.005.2013303130737.hdf
# @param resultFile Name of the result file
# @param bandTime Time when the image/band was taken
# @param return the name of the resultFile
.processBand <- function(bandPath, fileName, resultFile, bandTime) {
  u <- new("Util")
  fileExt <- getFileExtension(u, fileName)
  tmpFilename <- paste(resultFile, "_tmp", sep = "")
  
  if(fileExt  == "hdf"){# HDF file
    #Extract the bands to txt files
    cmdgdal2xyz <- paste("gdal2xyz.py -csv ", bandPath, tmpFilename, sep = " ")
    system(command = cmdgdal2xyz, intern = TRUE, wait = TRUE, ignore.stdout = TRUE, ignore.stderr = TRUE)
    
    #Change the coords by indexes and adds the date as the 3rd column
    modisTileId <- getTileIdFromFilename(u, fileName = fileName)
    ImFirstPixel <- getFirstGmip(u, modisTileId = modisTileId, nrows = 4800, ncols = 4800)
    #awk -F, '{$(NF-2)=((FNR - 1) % 4800) + 52800;} {$(NF-1)=int((FNR - 1) / 4800) + 48000;} {$(NF)="19010101" FS $(NF);}1' OFS=, MOD09Q1.A2013321.h10v08.005.2013332223240bandMOD_Grid_250m_Surface_Reflectancesur_refl_b01.txt_tmp > resfile.txt
    cmdawk <- paste("awk -F, '{$(NF-2)=((FNR - 1) % 4800) + ", ImFirstPixel[1], ";} {$(NF-1)=int((FNR - 1) / 4800) + ", ImFirstPixel[2], ";} {$(NF)=\"", bandTime, "\" FS $(NF);}1' OFS=, ", tmpFilename, " > ", resultFile, sep = "")  
    system(command = cmdawk, intern = TRUE, wait = TRUE)#, ignore.stdout = TRUE, ignore.stderr = TRUE)
    file.remove(tmpFilename)
  }else if(fileExt  == "nc"){# NetCDF file
    tmpPathParts <- unlist(strsplit(bandPath, "/"))
    bandNumber <- tmpPathParts[length(tmpPathParts)]
    fname <- substr(bandPath, 1, (nchar(bandPath) - (nchar(bandNumber) + 1)))
    #Extract the bands to txt files
    cmdgdal2xyz <- paste("gdal2xyz.py -csv -band", bandNumber, fname, tmpFilename, sep = " ")
    system(command = cmdgdal2xyz, intern = TRUE, wait = TRUE, ignore.stdout = TRUE, ignore.stderr = TRUE)
    #Change the coords by indexes and adds the date as the 3rd column
    cmdawk <- paste("awk -F, '{$(NF-2)=719.5 + $1/0.25;} {$(NF-1)=359.5 + $2/0.25;} {$(NF)=\"", bandTime, "\" FS $(NF);}1' OFS=, ", tmpFilename, " > ", resultFile, sep = "")
    system(command = cmdawk, intern = TRUE, wait = TRUE)#, ignore.stdout = TRUE, ignore.stderr = TRUE)
    file.remove(tmpFilename)
  }
  return(resultFile)
}

# Process a NetCDF file. NetCDF files can contain several bands
#
# @param filePath Path to the file
# @param resultFolder Path to the folder where to store the resulting files
# @return A character vector with the paths to the result files
.processNcdf <- function(filePath, resultFolder){
  #TODO: Improve dates
  ncdf <- raster(filePath)
  totalBands <- c(1:nbands(ncdf))
  
  # Each file contains data from 1995 to 2013 and 6940 bands. Assuming bands are numered starting at 1 for 1995-01-01 we have:
  start <- as.Date("2003-01-01")
  end <- as.Date("2004-12-31")    
  refDate <- as.Date("1995-01-01")
  refStart <- as.numeric(start - refDate) + 1
  refEnd <- as.numeric(end - refDate) + 1
  bands <- totalBands[refStart:refEnd]
  n <- c(1:length(bands))
  
  fileName <- .getFilenameFromFilepath(filePath)
  bandPaths <- paste(filePath, bands, sep = "/")
  resultFiles <- vector(mode = "character", length = length(bands))
  bandTimes <- vector(mode = "numeric", length = length(bands))
  for(i in n){
    resultFilename <- .getFileresultFromFilename(fileName = fileName, band = bands[i], ext = ".txt")
    resultFiles[i] <- paste0(resultFolder, "/", resultFilename)
    band <- raster(filePath, band = bands[i])
    bandTimes[i] <- .processTime(slot(band,"z")[[1]])
  }
  res <- unlist(mclapply(n, .dummy_processXXX, bandPaths = bandPaths, fileName = fileName, resultFiles= resultFiles, bandTimes = bandTimes))
  return (res)
  
}


#*******************************************************
#UTIL
#*******************************************************


# Dummy function for LAPPLY in .processHDF or .processNcdf
#
# @param n Vector numeric with indexes to the input vectors
# @param bandPaths Vector character of the full paths to the bands inside the files
# @param fileName Name of the file (No path but include extension)
# @param resultFiles Vector character with full paths to the files for storing the results
# @param modisGrid A ModisGrid object
# @param bandTimes Vector numeric with the data when the images/bands were taken
# @return A character with the path to the result file
.dummy_processXXX <- function(n, bandPaths, fileName, resultFiles, bandTimes){
  res <- .processBand(bandPath = bandPaths[n], 
                      fileName = fileName, 
                      resultFile = resultFiles[n], 
                      bandTime = bandTimes[n]) 
  return (res)
}

