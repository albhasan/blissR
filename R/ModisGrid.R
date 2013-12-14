#' The MODISGRID class
#'
#' Use this class for dealing with MODIS Grid related stuff.
#'
#' You can ???????
#'
#'
#' @note No notes
#' @name ModisGrid
#' @aliases ModisGrid-class
#' @exportClass ModisGrid
#' @author Alber Sanchez
setClass(
  Class = "ModisGrid", 
  slots = c(modisGridBoundaries = "matrix"),
  validity = function(object){
    #cat("~~~ ModisGrid: inspector ~~~ \n")
    res <- TRUE
    if(ncol(object@modisGridBoundaries) != 6 && nrow(object@modisGridBoundaries) != 648)
      res <- FALSE
    if(res == FALSE)
      stop ("[ModisGrid: validation] Some parameters are invalid")
    return(res)
  }
)



#*******************************************************
#CONSTRUCTOR
#*******************************************************
setMethod(
  f="initialize",
  signature="ModisGrid",
  definition=function(.Object){
    #cat ("~~~~~ ModisGrid: initializator ~~~~~ \n")
    .Object@modisGridBoundaries <-  .getBoundariesFromFile()
    validObject(.Object)# call of the inspector
    return(.Object)
  }
)


#*******************************************************
#ACCESSORS
#*******************************************************

#' Returns the object's MODIS grid boundaries
#' 
#' @param object An object of the type ModisGrid
#' @docType methods
#' @export 
setGeneric("getModisGridBoundaries",function(object){standardGeneric ("getModisGridBoundaries")})
setMethod("getModisGridBoundaries","ModisGrid",
          function(object){
# @param object A ModisGrid object
            return(object@modisGridBoundaries)
          }
)





#*******************************************************
#GENERIC METHODS
#*******************************************************

#*******************************************************
#METHODS
#*******************************************************

#' Calculates the coordinates for a subset of rows of a raster
#' 
#' @param object An object of the type ModisGrid
#' @param xRes Image resolution in the x direction
#' @param yRes Image resolution in the y direction
#' @param xmin Minimum x coordinate of the whole image
#' @param ymax Maximum y coordinate of the whole image
#' @param ncols Total number of columns in the image
#' @param nrows Number of consecutive rows taken from the image
#' @param startRow Index of the first row in the row subset
#' @return A list containing 2 numeric vectors with the x and y coordinates
#' @export 
setGeneric(name = "calculateRowSubsetCoords", def = function(object, xRes, yRes, xmin, ymax, ncols, nrows, startRow){standardGeneric("calculateRowSubsetCoords")})
setMethod(
  f = "calculateRowSubsetCoords",
  signature = "ModisGrid",
  definition = function(object, xRes, yRes, xmin, ymax, ncols, nrows, startRow){
    res <- .calculateRowSubsetCoords(xRes = xRes, yRes = yRes, xmin = xmin, ymax = ymax, ncols = ncols, nrows = nrows, startRow = startRow)
    return (res)
  }
)

#' Calculates the pixel coordinates for a subset of rows of a raster
#'
#' @param object An ModisGrid object
#' @param nrows Total number of rows in the image
#' @param ncols Total number of columns in the image
#' @param nsubsetrows Number of consecutive rows taken from the image
#' @param startRow Index of the first row in the row subset
#' @return A list containing 2 numeric vectors with the pixel's x and y coordinates
#' @export 
setGeneric(name = "calculateRowSubsetPixelCoords", def = function(object, nrows, ncols, nsubsetrows, startRow){standardGeneric("calculateRowSubsetPixelCoords")})
setMethod(
  f = "calculateRowSubsetPixelCoords",
  signature = "ModisGrid",
  definition = function(object, nrows, ncols, nsubsetrows, startRow){
# @param object An object of the type ModisGrid
    res <- .calculateRowSubsetPixelCoords(nrows = nrows, ncols = ncols, nsubsetrows = nsubsetrows, startRow = startRow)
    return (res)
  }
)



#' Takes pixel coordinates and displace them to fit the Global Modis Index
#'
#' @param object An ModisGrid object
#' @param modisTileId A character with a MODIS tile id (i.e "h10v08")
#' @param nrows Number of rows in a MODIS image (i.e for MOD09Q1 is 4800)
#' @param ncols Number of rows in a MODIS image (i.e for MOD09Q1 is 4800)
#' @param iLocalCoords Numeric coords of the pixel ids local to the image
#' @param jLocalCoords Numeric coords of the pixel ids local to the image
#' @return A list with 2 elements: The resulting i and j coords
#' @export 
setGeneric(name = "displacePixelToGmpi", def = function(object, modisTileId, nrows, ncols, iLocalCoords, jLocalCoords){standardGeneric("displacePixelToGmpi")})
setMethod(
  f = "displacePixelToGmpi",
  signature = "ModisGrid",
  definition = function(object, modisTileId, nrows, ncols, iLocalCoords, jLocalCoords){
# @param object An object of the type ModisGrid
    res <- .displacePixelToGmpi(modisTileId = modisTileId, nrows = nrows, ncols = ncols, iLocalCoords = iLocalCoords, jLocalCoords = jLocalCoords)
    return (res)
  }
)


#*******************************************************
#WORKER
#*******************************************************

# Builds a matrix containing the Global MODIS Pixel Indexes for the given MODIS tile
#
# @param modisTileId A character with a MODIS tile id (i.e "h10v08")
# @param nrows Number of rows in a MODIS image (i.e for MOD09Q1 is 4800)
# @param ncols Number of rows in a MODIS image (i.e for MOD09Q1 is 4800)
# @return A numeric matrix where the first colum contains the indexes in the x-direction and the second in y-direction
.buildGmpi <- function(modisTileId, nrows, ncols){
  ijvectors <- .buildij(nrows = nrows, ncols = ncols)
  iPixelImg <- ijvectors[[1]]
  jPixelImg <- ijvectors[[2]]
  thtv <- as.numeric(.getHV(modisTileId))
  firstGmpiPixel <- .getFirstGmip(modisTileId, nrows, ncols)
  iGpid <- iPixelImg + firstGmpiPixel[1]
  jGpid <- jPixelImg + firstGmpiPixel[2]
  res <- cbind(iGpid, jGpid)
}


# Takes pixel coordinates and displace them to fit the Global Modis Index
#
# @param modisTileId A character with a MODIS tile id (i.e "h10v08")
# @param nrows Number of rows in a MODIS image (i.e for MOD09Q1 is 4800)
# @param ncols Number of rows in a MODIS image (i.e for MOD09Q1 is 4800)
# @param iLocalCoords Numeric coords of the pixel ids local to the image
# @param jLocalCoords Numeric coords of the pixel ids local to the image
# @return A list with 2 elements: The resulting i and j coords
.displacePixelToGmpi <- function(modisTileId, nrows, ncols, iLocalCoords, jLocalCoords){
  firstGmpiPixel <- .getFirstGmip(modisTileId, nrows, ncols)
  itmp <- iLocalCoords + firstGmpiPixel[1]
  jtmp <- jLocalCoords + firstGmpiPixel[2]
  res <- list(itmp, jtmp)
  return(res)
}


# Calculates the pixel coordinates for a subset of rows of a raster
#
# @param nrows Total number of rows in the image
# @param ncols Total number of columns in the image
# @param nsubsetrows Number of consecutive rows taken from the image
# @param startRow Index of the first row in the row subset
# @return A list containing 2 numeric vectors with the pixel's x and y coordinates
.calculateRowSubsetPixelCoords <- function(nrows, ncols, nsubsetrows, startRow){
  ijvectors <- .buildij(nrows = nrows, ncols = ncols)
  iPixelImg <- ijvectors[[1]]
  jPixelImg <- ijvectors[[2]]
  jPixelImg <- jPixelImg[startRow:(startRow + nsubsetrows - 1)]
  res <- list(iPixelImg, jPixelImg)
  return(res)
}

#Calculates the coordinates for a subset of rows of a raster
#
# @param xRes Image resolution in the x direction
# @param yRes Image resolution in the y direction
# @param xmin Minimum x coordinate of the whole image
# @param ymax Maximum y coordinate of the whole image
# @param ncols Total number of columns in the image
# @param nrows Number of consecutive rows taken from the image
# @param startRow Index of the first row in the row subset
# @return A list containing 2 numeric vectors with the x and y coordinates
.calculateRowSubsetCoords <- function(xRes, yRes, xmin, ymax, ncols, nrows, startRow){
  xstart = xmin + (0.5 * xRes)
  ystart = (ymax - (0.5 * yRes)) - (yRes * (startRow - 1))
  xCoordsV <- rep(seq(from = xstart, by = xRes, length.out = ncols), times = nrows)
  yCoordsV <- rep(seq(from = ystart, by = -yRes, length.out = nrows), each = ncols)
  res <- list(xCoordsV, yCoordsV)
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
  iGpid <- thtv[1] * nrow
  jGpid <- thtv[2] * ncol
  res <- c(iGpid, jGpid)
}

#*******************************************************
#UTIL
#*******************************************************

# Build numeric vector containing the pixel indexes of a single image
#
# @param nrows Total number of rows in the image
# @param ncols Total number of columns in the image
# @return A list with two numeric vectors (i and j indexes starting at 0)
.buildij <- function(nrows, ncols){
  iPixelImg <- seq(from = 0, to = (ncols - 1), by = 1)
  jPixelImg <- seq(from = 0, to = (nrows - 1), by = 1)
  res <- list(iPixelImg, jPixelImg)
  return(res)
}

# Reads the file with the lat-long boundaries of MODIS tiles
#
# @return A data.frame
.getBoundariesFromFile <- function(){
  #NOTE: R como lenguaje de programacion es un puto pedazo de mierda!!!!!
  #¿El muy mal nacido no acepta "data.frame" como tipo de dato de un slot? entonces, ¿para qué hijueputas tienen ese objeto?
  path <- system.file("/data/modisGridBoundaries.txt", package="blissR", mustWork = TRUE)
  res <- as.matrix(read.table(path, header = TRUE, na.strings = c("-999.0000", "-99.0000")))
  return(res)
}

# Returns the boundaries of a MODIS tile
#
# @param modisTileId A character with a MODIS tile id (i.e "h10v08")
# @return A data.frame with tile information including the boundaries
.getModisTileBoundary <- function(modisTileId){
  modisBoundaries <- data(modisGridBoundaries)
  thtv <- as.numeric(.getHV(modisTileId))
  res <- modisBoundaries[modisBoundaries$ih == thtv[1] & modisBoundaries$iv == thtv[2],]
  return(res)
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


# Wrapper for MODIS::getTile. It returns the MODIS grid tile where the given latitude and longitude are located
#
# @param lon Numeric value of the longitude
# @param lat Numeric value of the latitude
# @return A character with a MODIS tile id (i.e "h10v08")
.getModisTileId <- function(lon, lat){
  res <- getTile(extent = list(xmin = lon, xmax = lon, ymax = lat, ymin = lat), tileH = NULL, tileV = NULL, buffer = NULL, system = "MODIS", zoom = FALSE)
  return (unlist(res[1]))
}

