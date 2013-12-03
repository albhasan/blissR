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
  slots = c(localArcPath = "character", 
            resultFolder = "character"),
  validity = function(object){
    cat("~~~ ModisProcessor: inspector ~~~ \n")
    res <- TRUE
    if(nchar(object@localArcPath) < 2)
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
  definition=function(.Object, localArcPath, resultFolder){
    cat ("~~~~~ ModisProcessor: initializator ~~~~~ \n")
    .Object@localArcPath <- localArcPath
    .Object@resultFolder <- resultFolder
    validObject(.Object)# call of the inspector
    return(.Object)
  }
)

#*******************************************************
#ACCESSORS
#*******************************************************
setGeneric("getLocalArcPath",function(object){standardGeneric ("getLocalArcPath")})
setMethod("getLocalArcPath","ModisProcessor",
          function(object){
            return(object@localArcPath)
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

#' Process MODIS' hdf files
#' 
#' @return A boolean indicating success or failure
#' @docType methods
#' @rdname processHdfs-methods
#' @export 
setGeneric(name = "processHdfs", def = function(object){standardGeneric("processHdfs")})
setMethod(
  f = "processHdfs",
  signature = "ModisProcessor",
  definition = function(object){
    
    res <- .processHdfs(getLocalArcPath(object), getResultFolder(object))  
    return(res)
    
  }
)


#*******************************************************
#WORKER
#*******************************************************



# Process the HDF files dowloaded from NASA's website
#
# @ param localArcPath Path to the folder taht will contain the MODIS files (See modis package documentation)
.processHdfs <- function(localArcPath, resultFolder){
  
  files <- list.files(path = localArcPath, pattern = "*.hdf", full.names = TRUE, recursive = TRUE, include.dirs = TRUE)
  mclapply(files, .processHdf, resultFolder)
  
}

# Process a HDF file. HDF-EOS files can contain several bands
#
# @param hdfFilePath Path to the HDF file
# @param resultFolder Path to the folder where to store the resulting files
.processHdf <- function(hdfFilePath, resultFolder){
  tmpFilePathParts <- unlist(strsplit(hdfFilePath, split = "/"))
  hdfFileName <- tmpFilePathParts[length(tmpFilePathParts)]
  imageSds <- getSds(hdfFilePath)
  tmpFilename = ''
  
  tmpFileNameParts <- unlist(strsplit(hdfFileName, split = "[.]"))
  tmpOutFileName <- paste(tmpFileNameParts[-6],  collapse = '')
  resultFiles <- rep(paste0(resultFolder,"/",tmpOutFileName), times = length(imageSds[[1]]))
  resultFiles <- paste0(resultFiles,imageSds[[1]])
  resultFiles <- paste(resultFiles, rep("txt", length(imageSds[[1]])), sep=".")
  
  l <- as.list(1:length(imageSds[[1]]))
  lapply(l, .dummy_processHdf, imageSds = imageSds, hdfFileName = hdfFileName, resultFiles = resultFiles)
  
}

# Dummy function for LAPPLY in processHdf
#
# @param n Index for selecting from lists
# @param imageSds List resulting of applying the raster::getSds function to the HDF file
# @param hdfFileName Name of the HDF file
# @param resultFiles Vector with paths to the files for storing the results
.dummy_processHdf <- function(n, imageSds, hdfFileName, resultFiles){
  hdfBandPath <- get("SDS4gdal", imageSds)[n]
  resultFile <- resultFiles[n]
  .processHdfBand(hdfBandPath, hdfFileName, resultFile)
}


#Process a single band of a HDF file
#
# @param hdfBandPath This is the imageSds of a specific band in the HDF file. i.e HDF4_EOS:EOS_GRID:/mnt/lun0/MODIS_ARC/MODIS/MOD09Q1.005/2013.10.08/MOD09Q1.A2013281.h11v09.005.2013303130737.hdf:MOD_Grid_250m_Surface_Reflectance:sur_refl_b01
# @param hdfFileName Name of the HDF file. i.e MOD09Q1.A2013281.h11v09.005.2013303130737.hdf
# @param resultFile Name of the result file
# @param tmpFilename OPTIONAL name of a temporal file
.processHdfBand <- function(hdfBandPath, hdfFileName, resultFile, tmpFilename = '') {
  inputRaster <- raster(hdfBandPath)
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
      tmp <- .getStuffTogether(v, rowIndex, rowAmount, ncols, hdfFileName, imgExtent, imgResolution)
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
      tmp <- .getStuffTogether(v, rowIndex, rowAmount, ncols, hdfFileName, imgExtent, imgResolution)
      if(i == 1){
        write.table(tmp, file = resultFile, append = TRUE, sep = ",", row.names = FALSE, col.names = TRUE, quote = FALSE)
      }else{
        write.table(tmp, file = resultFile, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE, quote = FALSE)  
      }
    }
  }
}


#Creates an array out of set of rows of a raster
#
# @param v Vector with the image values
# @param row Index of the first row of image data contained in v
# @param nrows Number of rows which data is contained in v
# @param ncols Number of columns in the image
# @param hdfFileName Name of the HDF file
# @param imgExtent Extent object (raster package) of the image
# @param imgResolution Vector with the image resolution in x and y axis
# @return A data.frame where each row is  pixel with columns x, y, time (Acquisition date YYYYDDD), pixel value, x Pixel, y Pixel, HDF file name
.getStuffTogether <- function(v, row, nrows, ncols, hdfFileName, imgExtent, imgResolution){
  xImgCoordV <- rep(seq(from = 0, to = (ncols - 1), by = 1), times = nrows)
  yImgCoordV <- rep(seq(from = (row - 1), to = (row + nrows - 2), by = 1), each = ncols)
  fileV <- rep(hdfFileName, times = (nrows * ncols))
  tmpFileNameParts <- unlist(strsplit(hdfFileName, split = "[.]"))
  imageTime <- substr(tmpFileNameParts[2], 2, nchar(tmpFileNameParts[2]))
  tm <- as.numeric(imageTime)
  imgTimeV <- rep(tm, times = (nrows * ncols))
  
  xRes <- imgResolution[1]
  yRes <- imgResolution[2]
  xstart = xmin(imgExtent) + (0.5 * xRes)
  ystart = (ymax(imgExtent) - (0.5 * yRes)) - (yRes * (row - 1))
  xCoordsV <- rep(seq(from = xstart, by = xRes, length.out = ncols), times = nrows)
  yCoordsV <- rep(seq(from = ystart, by = -yRes, length.out = nrows), each = ncols)
  
  tmpNumCols <- 6
  resNum <- vector(mode = "numeric", length = (nrows * tmpNumCols))
  resNum <- append(xCoordsV, yCoordsV)
  resNum <- append(resNum, imgTimeV)
  resNum <- append(resNum, v)
  resNum <- append(resNum, xImgCoordV)
  resNum <- append(resNum, yImgCoordV)
  mtNum <- matrix(data = resNum, ncol = tmpNumCols, byrow = FALSE)
  res <- as.data.frame(mtNum)
  res$filename <- fileV
  colnames(res) <- c("xCoord", "yCoord", "tCoord", "value", "xImg", "yImg", "imgFile")
  return (res)
}