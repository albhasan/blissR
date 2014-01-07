#' The SCIDBINSTANCE class
#'
#' Use this class for doing stuff related to SciDB
#'
#'
#'@section Slots :
#'  \describe{
#'    \item{\code{host}:}{Object of class \code{"character"}, it is the name of the host of a SciDB instance.}
#'    \item{\code{port}:}{Object of class \code{"character"}, it is the number of the portof a SciDB instance.}
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
            port = "numeric"),
  validity = function(object){
    #cat("~~~ ScidbInstance: inspector ~~~ \n")
    res <- TRUE
    if(nchar(object@host) < 3)
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
#' @param object A ScidbInstance object
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


#' Insert one array into other
#'
#' @param object A ScidbInstance object
#' @param originArray Name of the source array
#' @param destinationArray Name of the target array
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


#' Sends an AQL query to SciDB
#'
#' @param object A ScidbInstance object
#' @param aql AQL expression
#' @docType methods
#' @export 
setGeneric(name = "queryAql", def = function(object, aql){standardGeneric("queryAql")})
setMethod(
  f = "queryAql",
  signature = "ScidbInstance",
  definition = function(object, aql){
    
    .connect(getHost(object), getPort(object))
    .queryAql(aql = aql)
  }
)


#' Sends an AQL query to SciDB using the OS instead of scidbR (Only works for local installations)
#'
#' @param object A ScidbInstance object
#' @param aql AQL expression
#' @docType methods
#' @export 
setGeneric(name = "queryAqlCmd", def = function(object, aql){standardGeneric("queryAqlCmd")})
setMethod(
  f = "queryAqlCmd",
  signature = "ScidbInstance",
  definition = function(object, aql){
    
    .connect(getHost(object), getPort(object))
    .queryAqlCmd(aql = aql)
  }
)


#' Sends an AFL query to SciDB and return the results using scidbR
#'
#' @param object A ScidbInstance object
#' @param afl AN AFL query
#' @param iterative Bollean. Control if the response is an iterator. See scidbR::iquery
#' @param ret Boolean. TRUE to return output. See scidb::iquery
#' @return The results or an iterator (when iterative = TRUE). See scidb::iquery 
#' @docType methods
#' @export 
setGeneric(name = "queryAfl", def = function(object, afl, iterative, ret){standardGeneric("queryAfl")})
setMethod(
  f = "queryAfl",
  signature = "ScidbInstance",
  definition = function(object, afl, iterative, ret){
    
    .connect(getHost(object), getPort(object))
    res <- .queryAfl(afl = afl, iterative = iterative, ret = ret)
    return(res)
  }
)

#' Checks if an array with the given name exists in SciDB
#'
#' @param object A ScidbInstance object
#' @param arrayName Vector with the names of the arrays
#' @return Logical vector with TRUE if the array exists, FALSE otherwise
#' @docType methods
#' @export 
setGeneric(name = "exist", def = function(object, arrayName){standardGeneric("exist")})
setMethod(
  f = "exist",
  signature = "ScidbInstance",
  definition = function(object, arrayName){
    
    .connect(getHost(object), getPort(object))
    res <- .exist(arrayName)
    return(res)
  }
)


#' Returns a valid SciDB's array name
#' 
#' @param object A ScidbInstance object
#' @param arrayName Name of the array to be validated
#' @return Character. A valid name
#' @docType methods
#' @export 
setGeneric(name = "getValidArrayName", def = function(object, arrayName){standardGeneric("getValidArrayName")})
setMethod(
  f = "getValidArrayName",
  signature = "ScidbInstance",
  definition = function(object, arrayName){
    
    res <- .getValidArrayName(arrayName = arrayName)
    return(res)
  }
)



#*******************************************************
#WORKER
#*******************************************************

# Returns a valid SciDB's array name
# 
# @param arrayName Name of the array to be validated
# @return Character. A valid name
.getValidArrayName <- function(arrayName){
  res <- gsub(".", "", arrayName, fixed = TRUE)
  res <- gsub("-", "_", res, fixed = TRUE)
  return(res)
}


#Returns the names of the currrent arrays in the SciDB instance
listArrays <- function(){
  res <- scidblist()
  return(res)
}


# Insert one array into other
#
# @param originArray Name of the source array
# @param destinationArray Name of the target array
.insert <- function(originArray, destinationArray){
  aql <- paste("INSERT INTO", destinationArray, "SELECT * FROM ", originArray, sep = " ")
  #TODO: Use the scidbR function instead of CMD
  #.queryAql(aql)
  .queryAqlCmd(aql)
}

# Deletes an array from SciDB
#
# @param arrayName Name of the array
.deleteArray <- function(arrayName){
  #TODO: Error handling
  scidbremove(arrayName)
}

# Sends an AQL query to SciDB
#
# @param aql AQL expression
.queryAql <- function(aql){
  iquery(aql, afl = FALSE, iterative = FALSE)
}

# Sends an AQL query to SciDB using the OS instead of scidbR (Only works for local installations)
#
# @param aql AQL expression
.queryAqlCmd <- function(aql){
  #TODO: Improve
  prefix <- "/opt/scidb/13.11/bin/iquery -q '"
  sufix <- "'"
  cmd <- paste(prefix, aql, sufix, sep="")
  system(cmd, intern = FALSE, wait = TRUE, ignore.stdout = TRUE, ignore.stderr = TRUE)
}


# Sends an AFL query to SciDB and return the results using scidbR
#
# @param afl AN AFL query
# @param iterative Bollean. Control if the response is an iterator. See scidbR::iquery
# @param ret Boolean. TRUE to return output. See scidb::iquery
.queryAfl <- function(afl, iterative, ret){
  res <- iquery(afl, afl = TRUE, iterative = iterative, `return` = ret)
  return (res)
}


# Checks if an array with the given name exists in SciDB
#
# @param arrayName Vector with the names of the arrays
# @return Logical vector with TRUE if the array exists, FALSE otherwise
.exist <- function(arrayName){
  alist <- scidblist()
  l <- length(arrayName)
  res <- vector(mode = "logical", length = l)
  for(i in 1:l){
    aname <- arrayName[i]
    res[i] <- aname %in% alist  
  }
  return(res)
}

# Connect to a SciDB datatbase
#
# @param host SciDB host
# @param port SciDB port
.connect <- function(host, port){
  if(is.null(port) || is.na(port)){
    scidbconnect(host = host)
  }else{
    scidbconnect(host = host, port = port)  
  }
}


# DEPRECATED Redimmnesion a SciDB array. It is better to use the load tools which load an redimmension in a single operation
#.arrayRedimmension <- function(arrayNameOrigin, arrayNameDestination){
#  arrayNames <- paste(arrayNameOrigin, arrayNameDestination, sep = ", ")
#  prefix <- "redimension_store("
#  sufix <- ");"
#  afl <- paste(prefix, arrayNames, sufix, sep = "")
#  res <- .queryAfl(afl = afl, iterative = FALSE, ret = FALSE)
#}

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


#*******************************************************
#UTIL
#*******************************************************


