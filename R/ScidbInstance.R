#' The SCIDBINSTANCE class
#'
#' Use this class for 
#'
#' You can ???????
#'
#'@section Slots :
#'  \describe{
#'    \item{\code{files}:}{Object of class \code{"character"}, it is a vector with the paths to the files.}
#'  }
#'
#' @note No notes
#' @name ScidbInstance
#' @aliases ScidbInstance-class
#' @exportClass ScidbInstance
#' @author Alber Sanchez
setClass(
  Class = "ScidbInstance", 
  slots = c(host = "character",
            port = "character"),
  validity = function(object){
    #cat("~~~ ScidbInstance: inspector ~~~ \n")
    res <- TRUE
    if(nchar(host) < 3)
      res <- FALSE
    if(res == FALSE)
      stop ("[ScidbInstance: validation] Some parameters are invalid")
    return(res)
  }
)

#*******************************************************
#CONSTRUCTOR
#*******************************************************
setMethod(
  f="initialize",
  signature="ScidbInstance",
  definition=function(.Object, host, port){
    #cat ("~~~~~ ScidbInstance: initializator ~~~~~ \n")
    .Object@host <- host
    .Object@port <- port
    validObject(.Object)# call of the inspector
    return(.Object)
  }
)

#*******************************************************
#ACCESSORS
#*******************************************************

#' Returns the object host
#' 
#' @param object A ScidbInstance object
#' @docType methods
#' @export 
setGeneric("getHost",function(object){standardGeneric ("getHost")})
setMethod("getHost","ScidbInstance",
          function(object){
            return(object@host)
          }
)

#' Returns the object port
#' 
#' @param object A ScidbInstance object
#' @docType methods
#' @export 
setGeneric("getPort",function(object){standardGeneric ("getPort")})
setMethod("getPort","ScidbInstance",
          function(object){
            return(object@port)
          }
)



#*******************************************************
#GENERIC METHODS
#*******************************************************

#*******************************************************
#METHODS
#*******************************************************

#' Deletes an array from SciDB
#' 
#' @param object A ModisDownloader object
#' @param arrayName Name of the array to be deleted
#' @docType methods
#' @export 
setGeneric(name = "deleteArray", def = function(object, arrayName){standardGeneric("deleteArray")})
setMethod(
  f = "deleteArray",
  signature = "ScidbInstance",
  definition = function(object, arrayName){
    
    .connect(getHost(object), getPort(object))
    .deleteArray(arrayName)

  }
)


#' Create an array of 1 dimmension in SciDB
#' 
#' @param object A ModisDownloader object
#' @param arrayName Name of the array to be created
#' @docType methods
#' @export 
setGeneric(name = "create1DModisArray", def = function(object, arrayName){standardGeneric("create1DModisArray")})
setMethod(
  f = "create1DModisArray",
  signature = "ScidbInstance",
  definition = function(object, arrayName){

    .connect(getHost(object), getPort(object))
    .create1DModisArray(arrayName = arrayName)
  }
)


#' Create an array of 3 dimmension in SciDB
#' 
#' @param object A ModisDownloader object
#' @param arrayName Name of the array to be created
#' @docType methods
#' @export 
setGeneric(name = "create3DModisArray", def = function(object, arrayName){standardGeneric("create3DModisArray")})
setMethod(
  f = "create3DModisArray",
  signature = "ScidbInstance",
  definition = function(object, arrayName){
    
    .connect(getHost(object), getPort(object))
    .create3DModisArray(arrayName = arrayName)
  }
)


#' Insert array data into another array
#' 
#' @param object A ModisDownloader object
#' @param arrayName Name of the array to be created
#' @docType methods
#' @export 
setGeneric(name = "insert", def = function(object, originArray, destinationArray){standardGeneric("insert")})
setMethod(
  f = "insert",
  signature = "ScidbInstance",
  definition = function(object, originArray, destinationArray){

    .connect(getHost(object), getPort(object))
    .insert(originArray = originArray, destinationArray = destinationArray)
  }
)


  



#*******************************************************
#WORKER
#*******************************************************

#.arrayRedimmension <- function(arrayNameOrigin, arrayNameDestination){
#redimension_store(device_probe, two_dim);  
#}


.insert <- function(originArray, destinationArray){
  aql <- paste("INSERT INTO", destinationArray, "SELECT * FROM ", originArray, sep = " ")
  #.queryAql(aql)
  .queryAqlCmd(aql)
}



# DEPRECATED
#redimAndInsert <- function(originArray, destinationArray){
  ## 1 - Redimension origin 1D array into a temporal 3D array
  #tmp3Darrayname <- paste(originArray, "_3D", sep = "")
  #.create3DModisArray(tmp3Darrayname)
  ##aqlRedim <- paste("SELECT * INTO", tmp3Darrayname, "FROM", originArray, sep = " ") 
  ##aqlRedim <- paste(aqlRedim, ";", sep = " ")
  ##.queryAql(aqlRedim)
  #aflRedim <- paste("redimension_store(", originArray, ",", tmp3Darrayname, ");", sep = " ")#redimension_store(originArray, tmp3Darrayname);  
  #.queryAfl(aflRedim, iterative = FALSE, ret = FALSE)
  ## 2  - Insert tmp 3D array into destination array
  #aqlInsert <- paste("INSERT INTO", destinationArray, "(SELECT * FROM ", tmp3Darrayname, ");", sep = " ")
  #.queryAql(aqlInsert)
#}

.create1DModisArray <- function(arrayName){
  prefix <- "CREATE ARRAY"
  at <- "<i:int64, j:int64, t:int64, value:double>"
  di  <- "[k=0:*,1000000,0]"
  aql <- paste(prefix, arrayName, at, di, ";", sep = " ")
  .queryAql(aql)
}


.create3DModisArray <- function(arrayName){
  prefix <- "CREATE ARRAY"
  at <- "<value:double>"
  di  <- "[i=0:172799,1000,2, j=0:86399,1000,2, t=19900000:20200000,1000,0]"
  aql <- paste(prefix, arrayName, at, di, ";", sep = " ")
  .queryAql(aql)
}


.deleteArray <- function(arrayName){
  #TODO: Error handling
  scidbremove(arrayName)
}

.queryAql <- function(aql){
  iquery(aql, afl = FALSE, iterative = FALSE)
}


.queryAqlCmd <- function(aql){
  prefix <- "/opt/scidb/13.11/bin/iquery -q '"
  sufix <- "'"
  cmd <- paste(prefix, aql, sufix, sep="")
  system(cmd, intern = TRUE)
}





.queryAfl <- function(afl, iterative, ret){
  res <- iquery(aql, afl = TRUE, iterative = iterative, `return` = ret)
  return (res)
}

.arrayRedimmension <- function(arrayNameOrigin, arrayNameDestination){
  arrayNames <- paste(arrayNameOrigin, arrayNameDestination, sep = ", ")
  prefix <- "redimension_store("
  sufix <- ");"
  afl <- paste(prefix, arrayNames, sufix, sep = "")
  res <- .queryAfl(afl = afl, iterative = FALSE, ret = FALSE)
}



#Check if an array with the given name exists
.exist <- function(arrayName){
  alist <- scidblist()
  res <- arrayName %in% alist
  return(res)
}



# Creates the SciDB arrays
#
# @param force Force the creation. Makes sure the arrays are empty
.createModisArrays <- function(force){
  ma1d <- c("loadMOD09Q1sur_refl_b01_1D", "loadMOD09Q1sur_refl_b02_1D", "loadMOD09Q1sur_refl_qc_250m_1D")
  ma3d <- c("MOD09Q1sur_refl_b01", "MOD09Q1sur_refl_b02", "MOD09Q1sur_refl_qc_250m")
  
  for(arrayName in ma1d){
    if(.exist(arrayName) == FALSE){
      .create1DModisArray(arrayName)
    }else{
      if(force == TRUE){
        .deleteArray(arrayName)
        .create1DModisArray(arrayName)
      }
    }
  }
  for(arrayName in ma3d){
    if(.exist(arrayName) == FALSE){
      .create3DModisArray(arrayName)
    }else{
      if(force == TRUE){
        .deleteArray(arrayName)
        .create3DModisArray(arrayName)
      }
    }
  }
}

#*******************************************************
#UTIL
#*******************************************************
.connect <- function(host, port){
  if(is.null(port)){
    scidbconnect(host = host)
  }else{
    scidbconnect(host = host, port = port)  
  }
}

