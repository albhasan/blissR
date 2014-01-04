#' The LOADER class
#'
#' Use this class for loading processed files into SciDB
#'
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
#' @name Loader
#' @aliases Loader-class
#' @exportClass Loader
#' @author Alber Sanchez
setClass(
  Class = "Loader", 
  slots = c(files = "character", 
            scidbhost = "character",
            scidbport = "numeric",
            scidbInstance = "ScidbInstance"),  
  validity = function(object){
    #cat("~~~ Loader: inspector ~~~ \n")
    res <- TRUE
    if(length(object@files) < 1)
      res <- FALSE
    if(length(object@scidbhost) < 1)
      res <- FALSE
    if(is.null(object@scidbInstance)){
      #res <- FALSE
      cat("Loader: ScidbInstance is null")
    }
    if(res == FALSE)
      stop ("[Loader: validation] Some parameters are invalid")
    return(res)
  }
)

#*******************************************************
#CONSTRUCTOR
#*******************************************************
setMethod(
  f="initialize",
  signature="Loader",
  definition=function(.Object, files, scidbhost, scidbport){
    #cat ("~~~~~ Loader: initializator ~~~~~ \n")
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
#' @param object A Loader object
#' @docType methods
#' @export 
setGeneric("getFiles",function(object){standardGeneric ("getFiles")})
setMethod("getFiles","Loader",
          function(object){
            return(object@files)
          }
)

#' Returns the object scidb host
#' 
#' @param object A Loader object
#' @docType methods
#' @export 
setGeneric("getScidbhost",function(object){standardGeneric ("getScidbhost")})
setMethod("getScidbhost","Loader",
          function(object){
            return(object@scidbhost)
          }
)


#' Returns the object scidb port
#' 
#' @param object A Loader object
#' @docType methods
#' @export 
setGeneric("getScidbport",function(object){standardGeneric ("getScidbport")})
setMethod("getScidbport","Loader",
          function(object){
            return(object@scidbport)
          }
)

#' Returns the object scidb instance
#' 
#' @param object A Loader object
#' @docType methods
#' @export 
setGeneric("getScidbInstance",function(object){standardGeneric ("getScidbInstance")})
setMethod("getScidbInstance","Loader",
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
#' @param object A Loader object
#' @param keepLoadData If TRUE keeps the data loaded (1D array) in an array inside SciDB
#' @return A character vector
#' @docType methods
#' @export 
setGeneric(name = "load", def = function(object, keepLoadData){standardGeneric("load")})
setMethod(
  f = "load",
  signature = "ModisLoader",
  definition = function(object, keepLoadData){
    
    files = getFiles(object)
    #filter the input vector according to its name to know the destination array
    #Modis
    modisFilesB1 <- files[grep("refl_b01.txt", files)]
    modisFilesB2 <- files[grep("refl_b02.txt", files)]
    modisFilesBc <- files[grep("refl_qc_250m.txt", files)]
    #NetCDF
    ncdfFilesrr <- files[grep("rr_0.25deg_reg_1995-2013_v9.0.nc", files)]
    ncdfFilestg <- files[grep("tg_0.25deg_reg_1995-2013_v9.0.nc", files)]
    
    #Modis
    destination1DArray_b1 <- "loadMOD09Q1sur_refl_b01_1D"
    destination3DArray_b1 <- "MOD09Q1sur_refl_b01"
    destination1DArray_b2 <- "loadMOD09Q1sur_refl_b02_1D"
    destination3DArray_b2 <- "MOD09Q1sur_refl_b02"
    destination1DArray_bc <- "loadMOD09Q1sur_refl_qc_250m_1D"
    destination3DArray_bc <- "MOD09Q1sur_refl_qc_250m"
    #NetCDF
    destination1DArray_rr <- "loadrr_025deg_reg_19952013_v9"
    destination3DArray_rr <- "rr_025deg_reg_1995-2013_v90"
    destination1DArray_tg <- "loadtg_025deg_reg_19952013_v90"
    destination3DArray_tg <- "tg_025deg_reg_19952013_v90"
    
    
    
    scidbInstance = getScidbInstance(object)
    
    #Makes sure the arrays exist
    .createModisArrays(scidbInstance = scidbInstance, f = FALSE)
    
    #Loads the data
    resMb1 <- .load(files = modisFilesB1, destination1DArray = destination1DArray_b1, destination3DArray = destination3DArray_b1, keepLoadData = keepLoadData, scidbInstance = scidbInstance)
    resMb2 <- .load(files = modisFilesB2, destination1DArray = destination1DArray_b2, destination3DArray = destination3DArray_b2, keepLoadData = keepLoadData, scidbInstance = scidbInstance)
    resMbc <- .load(files = modisFilesBc, destination1DArray = destination1DArray_bc, destination3DArray = destination3DArray_bc, keepLoadData = keepLoadData, scidbInstance = scidbInstance)
    
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
  
  u <- new("Util")
  filename <- getFilenameFromFilepath(u, filepath = filepath)
  filenameNoExt <- getFileNoExtension(u, filename)
  #create array 1Darray named as the text file
  loadArrayname <- paste("load_", filenameNoExt, sep = "")
  tmp3DArrayname <- paste("tmp_", filenameNoExt, sep = "")
  .create1DModisArray(arrayName = loadArrayname, scidbInstance = scidbInstance, f = TRUE)
  .create3DModisArray(arrayName = tmp3DArrayname, scidbInstance = scidbInstance, f = TRUE)
  #load
  cmd <- paste("sudo /opt/scidb/13.11/bin/./loadcsv.py -n 1 -t NNNN -a '", loadArrayname, "' -i ", filepath, " -A '", tmp3DArrayname, "'", sep = "")
  system (cmd)
  res <- c(loadArrayname, tmp3DArrayname)
  return(res)
}

# Exports the CSV files to SciDB format
#
# @param files A character vector with the path to csv files
# @return A character vector with the pat to the scidb files
.csv2scidb <- function(files){
  res <- vector(mode = "character", length = length(f))
  cmd <- list()
  u <- new("Util")
  for(i in 1:(length(files))){
    f <- files[i]
    filename <- getFilenameFromFilepath(u, f)
    path <- getFilepathFromFilepath(u, f)
    filenameNoExt <- getFileNoExtension(u, filename)
    filenameScidb <- paste(filenameNoExt, ".scidb", sep = "")
    filenameScidb <- paste(path, "/", filenameScidb, sep = "")
    cmd[i] <- paste("/opt/scidb/13.11/bin/csv2scidb -s 0 -p NNNN -i", f, "-o",filenameScidb, sep = " ")
    res[i] <- filenameScidb
  }
  tmp <- mclapply(cmd, system)
  return(res)
}

# Creates the SciDB arrays
#
# @param f Force the creation. Makes sure the arrays are empty
.createModisArrays <- function(scidbInstance, f){
  ma1d <- c("loadMOD09Q1sur_refl_b01_1D", "loadMOD09Q1sur_refl_b02_1D", "loadMOD09Q1sur_refl_qc_250m_1D")#,"loadrr_025deg_reg_19952013_v9", "loadtg_025deg_reg_19952013_v90")
  ma3d <- c("MOD09Q1sur_refl_b01", "MOD09Q1sur_refl_b02", "MOD09Q1sur_refl_qc_250m")#, rr_025deg_reg_19952013_v90", "tg_025deg_reg_19952013_v90")
  
  for(i in 1:(length(ma1d))){
    arrayName <- ma1d[i]
    .create1DModisArray(arrayName = arrayName, scidbInstance = scidbInstance, f = f)
  }
  for(i in 1:(length(ma3d))){
    arrayName <- ma3d[i]
    .create3DModisArray(arrayName = arrayName, scidbInstance = scidbInstance, f = f)
  }
  
}

# Create a SciDB array for storing MODIS data using a single unbounded dimmension
#
# @param arrayName Name of the array
# @param f Force the creation. Makes sure the array is empty
.create1DModisArray <- function(arrayName, scidbInstance, f){
  prefix <- "CREATE ARRAY"
  at <- "<i:int64, j:int64, t:int64, value:double>"
  di  <- "[k=0:*,1000000,0]"
  aql <- paste(prefix, arrayName, at, di, ";", sep = " ")
  
  e <- exist(scidbInstance, arrayName = arrayName)
  if(e == TRUE){
    if(f == TRUE){
      deleteArray(scidbInstance, arrayName = arrayName)
      queryAql(scidbInstance, aql = aql)  
    }else{}
  }else{
    queryAql(scidbInstance, aql = aql)  
  }
}

# Create a SciDB array for storing MODIS data using space and time dimensions
#
# @param arrayName Name of the array
# @param f Force the creation. Makes sure the array is empty
.create3DModisArray <- function(arrayName, scidbInstance, f){
  prefix <- "CREATE ARRAY"
  at <- "<value:double>"
  di  <- "[i=0:172799,1000,2, j=0:86399,1000,2, t=19900000:20200000,1000,0]"
  aql <- paste(prefix, arrayName, at, di, ";", sep = " ")
  
  e <- exist(scidbInstance, arrayName = arrayName)
  if(e == TRUE){
    if(f == TRUE){
      deleteArray(scidbInstance, arrayName = arrayName)
      queryAql(scidbInstance, aql = aql)  
    }else{}
  }else{
    queryAql(scidbInstance, aql = aql)  
  }
}


#*******************************************************
#UTIL
#*******************************************************
