#' The MODISLOADER class
#'
#' Use this class for loading to SciDB processed MODIS tiles
#'
#' You can ???????
#'
#'@section Slots :
#'  \describe{
#'    \item{\code{files}:}{Object of class \code{"character"}, it is a vector with the paths to the files.}
#'    \item{\code{scidbhost}:}{Object of class \code{"character"}, it is the name of the host of a SciDB instance.}
#'    \item{\code{scidbport}:}{Object of class \code{"character"}, it is the number of the portof a SciDB instance.}
#'    \item{\code{scidbInstance}:}{Object of class \code{"ScidbInstance"}, it holds the connection and related operations to a SciDB instance.}
#'  }
#'
#' @note No notes
#' @name ModisLoader
#' @aliases ModisLoader-class
#' @exportClass ModisLoader
#' @author Alber Sanchez
setClass(
  Class = "ModisLoader", 
  slots = c(files = "character", 
            scidbhost = "character",
            scidbport = "numeric",
            scidbInstance = "ScidbInstance"),  
  validity = function(object){
    #cat("~~~ ModisLoader: inspector ~~~ \n")
    res <- TRUE
    if(length(object@files) < 1)
      res <- FALSE
    if(length(object@scidbhost) < 1)
      res <- FALSE
    #if(length(object@scidbport) < 1)
    #  res <- FALSE
    #if(length(object@scidbInstance) < 1)
    #  res <- FALSE
    if(res == FALSE)
      stop ("[ModisLoader: validation] Some parameters are invalid")
    return(res)
  }
)

#*******************************************************
#CONSTRUCTOR
#*******************************************************
setMethod(
  f="initialize",
  signature="ModisLoader",
  definition=function(.Object, files, scidbhost, scidbport){
    #cat ("~~~~~ ModisLoader: initializator ~~~~~ \n")
    .Object@files <- files
    .Object@scidbhost <- scidbhost
    .Object@scidbport <- scidbport
    .Object@scidbInstance <- new("ScidbInstance", host = scidbhost, port = scidbport)
    validObject(.Object)# call of the inspector
    return(.Object)
  }
)

#*******************************************************
#ACCESSORS
#*******************************************************

#' Returns the object files
#' 
#' @param object A ModisLoader object
#' @docType methods
#' @export 
setGeneric("getFiles",function(object){standardGeneric ("getFiles")})
setMethod("getFiles","ModisLoader",
          function(object){
            return(object@files)
          }
)

#' Returns the object scidb host
#' 
#' @param object A ModisLoader object
#' @docType methods
#' @export 
setGeneric("getScidbhost",function(object){standardGeneric ("getScidbhost")})
setMethod("getScidbhost","ModisLoader",
          function(object){
            return(object@scidbhost)
          }
)


#' Returns the object scidb port
#' 
#' @param object A ModisLoader object
#' @docType methods
#' @export 
setGeneric("getScidbport",function(object){standardGeneric ("getScidbport")})
setMethod("getScidbport","ModisLoader",
          function(object){
            return(object@scidbport)
          }
)

#' Returns the object scidb instance
#' 
#' @param object A ModisLoader object
#' @docType methods
#' @export 
setGeneric("getScidbInstance",function(object){standardGeneric ("getScidbInstance")})
setMethod("getScidbInstance","ModisLoader",
          function(object){
            if(is.null(object@scidbInstance)){
              res <- new("ScidbInstance", host = object@scidbhost, port = object@scidbport)
            }else{
              res <- object@scidbInstance
            }
            return(res)
          }
)

#*******************************************************
#GENERIC METHODS
#*******************************************************

#*******************************************************
#METHODS
#*******************************************************

#' Loads the input files to SciDB and it deletes the source files
#' 
#' @param object A ModisLoader object
#' @return A character vector
#' @docType methods
#' @export 
setGeneric(name = "load", def = function(object){standardGeneric("load")})
setMethod(
  f = "load",
  signature = "ModisLoader",
  definition = function(object){
    
    files = getFiles(object)
    #filter the input vector according to its name to know the destination array
    modisFilesB1 <- files[grep("refl_b01.txt", files)]
    modisFilesB2 <- files[grep("refl_b02.txt", files)]
    modisFilesBc <- files[grep("refl_qc_250m.txt", files)]
    
    destination1DArray_b1 <- "loadMOD09Q1sur_refl_b01_1D"
    destination3DArray_b1 <- "MOD09Q1sur_refl_b01"
    destination1DArray_b2 <- "loadMOD09Q1sur_refl_b02_1D"
    destination3DArray_b2 <- "MOD09Q1sur_refl_b02"
    destination1DArray_bc <- "loadMOD09Q1sur_refl_qc_250m_1D"
    destination3DArray_bc <- "MOD09Q1sur_refl_qc_250m"
    
    scidbInstance = getScidbInstance(object)
    
    #Makes sure the arrays exist
    createModisArrays(scidbInstance, force = FALSE)
    
    #Loads the data
    resMb1 <- .load(files = modisFilesB1, destination1DArray = destination1DArray_b1, destination3DArray = destination3DArray_b1, scidbInstance = scidbInstance)
    resMb2 <- .load(files = modisFilesB2, destination1DArray = destination1DArray_b2, destination3DArray = destination3DArray_b2, scidbInstance = scidbInstance)
    resMbc <- .load(files = modisFilesBc, destination1DArray = destination1DArray_bc, destination3DArray = destination3DArray_bc, scidbInstance = scidbInstance)
    
    res <- NA
    return(res)
  }
)


#*******************************************************
#WORKER
#*******************************************************

# Loads a set of CSV files to SciDB
#
# @param files Vector character with the paths to the files
# @param destination1DArray Name of the 1 dimmnesion array in SciDB
# @param destination3DArray Name of the 3 dimmnesion array in SciDB
# @param scidbInstance An object of the class ScidbInstance
.load <- function(files, destination1DArray, destination3DArray, scidbInstance){

  #Loads the CSV files into SciDB
  tmpArrays <- mclapply(files, .loadFile, scidbInstance = scidbInstance)
  #redimmension the array to fit the destination array ()
  #insert into destination array
  for(i in 1:length(tmpArrays)){
    tmpArrayNames <- tmpArrays[[i]]
    loadArrayname <- tmpArrayNames[1]
    tmp3DArrayname <- tmpArrayNames[2]
    insert(scidbInstance, originArray = loadArrayname, destinationArray = destination1DArray)
    deleteArray(scidbInstance, arrayName = loadArrayname)
    insert(scidbInstance, originArray = tmp3DArrayname, destinationArray = destination3DArray)
    deleteArray(scidbInstance, arrayName = tmp3DArrayname)
  }
  file.remove(files)
}


# Loads a single file to SciDB. It creates a 1D array which later is redimmneioned in a 3D array
#
# @param filepath Path to the CSV file containing the data
# @param scidbInstance An object of the class ScidbInstance
# @return A vector containing the array namess of the 1D(load) and 3D arrays
.loadFile <- function(filepath, scidbInstance){
  
  filename <- .getFilenameFromFilepath(filepath = filepath)
  filenameNoExt <- .getFileNoExtension(filename)
  #create array 1Darray named as the text file
  loadArrayname <- paste("load_", filenameNoExt, sep = "")
  tmp3DArrayname <- paste("tmp_", filenameNoExt, sep = "")
  create1DModisArray(scidbInstance, arrayName = loadArrayname)
  create3DModisArray(scidbInstance, arrayName = tmp3DArrayname)
  #load
  cmd <- paste("sudo /opt/scidb/13.11/bin/./loadcsv.py -n 1 -t NNNN -a '", loadArrayname, "' -i ", filepath, " -A '", tmp3DArrayname, "'", sep = "")
  system (cmd)
  res <- c(loadArrayname, tmp3DArrayname)
  return(res)
}

# DEPRECATED Exports the CSV files to SciDB format
#
# @param files A character vector with the path to csv files
# @return A character vector with the pat to the scidb files
.csv2scidb <- function(files){
  res <- vector(mode = "character", length = length(f))
  cmd <- list()
  for(i in 1:(length(files))){
    f <- files[i]
    filename <- .getFilenameFromFilepath(f)
    path <- .getFilepathFromFilepath(f)
    filenameNoExt <- .getFileNoExtension(filename)
    filenameScidb <- paste(filenameNoExt, ".scidb", sep = "")
    filenameScidb <- paste(path, "/", filenameScidb, sep = "")
    cmd[i] <- paste("/opt/scidb/13.11/bin/csv2scidb -s 1 -p NNNN -i", f, "-o",filenameScidb, sep = " ")
    res[i] <- filenameScidb
   }
  tmp <- mclapply(cmd, system)
  return(res)
}


#*******************************************************
#UTIL
#*******************************************************

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

# Returns the filename of the path to the file
#
# @param filepath Character representing the full path to the file
# @return Character representing the filename including the file extension
.getFilenameFromFilepath <- function(filepath){
  filePathParts <- unlist(strsplit(filepath, split = "/"))
  res <- filePathParts[length(filePathParts)]
  return(res)
}

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