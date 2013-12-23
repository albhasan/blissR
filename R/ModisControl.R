#' The MODISCONTROL class
#'
#' Use this class for reviwing what has been loaded and processed
#'
#' You can ???????
#'
#'@section Slots :
#'  \describe{
#'    \item{\code{controlHost}:}{Object of class \code{"character"}, it is a parameter for connecting to a Database.}
#'    \item{\code{controlDbname}:}{Object of class \code{"character"}, it is a parameter for connecting to a Database.}
#'    \item{\code{controlUser}:}{Object of class \code{"character"}, it is a parameter for connecting to a Database.}
#'    \item{\code{controlPassword}:}{Object of class \code{"character"}, it is a parameter for connecting to a Database.}
#'  }
#'
#' @note No notes
#' @name ModisControl
#' @aliases ModisControl-class
#' @exportClass ModisControl
#' @author Alber Sanchez
setClass(
  Class = "ModisControl", 
  slots = c(controlHost = "character", 
            controlDbname = "character", 
            controlUser = "character", 
            controlPassword = "character"),
  validity = function(object){
    #cat("~~~ ModisControl: inspector ~~~ \n")
    res <- TRUE
    if(nchar(object@controlHost) < 1)
      res <- FALSE
    if(nchar(object@controlDbname) < 1)
      res <- FALSE
    if(nchar(object@controlUser) < 1)
      res <- FALSE
    if(nchar(object@controlPassword) < 1)
      res <- FALSE
    if(res == FALSE)
      stop ("[ModisControl: validation] Some parameters are invalid")
    return(res)
  }
)

#*******************************************************
#CONSTRUCTOR
#*******************************************************
setMethod(
  f="initialize",
  signature="ModisControl",
  definition=function(.Object, controlHost, controlDbbname, controlUser, controlPassword){
    #cat ("~~~~~ ModisControl: initializator ~~~~~ \n")
    .Object@controlHost <- controlHost
    .Object@controlDbname <- controlDbname
    .Object@controlUser <- controlUser
    .Object@controlPassword <- controlPassword
    validObject(.Object)# call of the inspector
    return(.Object)
  }
)

#*******************************************************
#ACCESSORS
#*******************************************************

#' Returns the object's host property
#' 
#' @param object A ModisControl object
#' @docType methods
#' @export 
setGeneric("getControlHost",function(object){standardGeneric ("getControlHost")})
setMethod("getControlHost","ModisControl",
          function(object){
            return(object@controlHost)
          }
)

#' Returns the object's dbname property
#' 
#' @param object A ModisControl object
#' @docType methods
#' @export 
setGeneric("getControlDbname",function(object){standardGeneric ("getControlDbname")})
setMethod("getControlDbname","ModisControl",
          function(object){
            return(object@controlDbname)
          }
)

#' Returns the object's user property
#' 
#' @param object A ModisControl object
#' @docType methods
#' @export 
setGeneric("getControlUser",function(object){standardGeneric ("getControlUser")})
setMethod("getControlUser","ModisControl",
          function(object){
            return(object@controlUser)
          }
)


#' Returns the object's user property
#' 
#' @param object A ModisControl object
#' @docType methods
#' @export 
setGeneric("getControlPassword",function(object){standardGeneric ("getControlPassword")})
setMethod("getControlPassword","ModisControl",
          function(object){
            return(object@controlPassword)
          }
)


#*******************************************************
#GENERIC METHODS
#*******************************************************

#*******************************************************
#METHODS
#*******************************************************


#' Checks if the given MODIS files has been uploaded
#' 
#' @param object A ModisLoader object
#' @param modisFiles A vector of MODIS HDF filenames
#' @return A character vector
#' @docType methods
#' @export 
 setGeneric(name = "isUploaded", def = function(object, modisFiles){standardGeneric("isUploaded")})
 setMethod(
   f = "isUploaded",
   signature = "ModisControl",
   definition = function(object, modisFiles){
     res <- .isUploaded(modisFiles = modisFiles, user = getControlUser(object), password = getControlPassword(object), dbname = getControlDbname(object), host = getControlHost(object))
     return(res)
   }
 )

#' Marks the given MODIS files as uploaded
#' 
#' @param object A ModisLoader object
#' @param modisFiles A vector of MODIS HDF filenames
#' @param isLoaded2tmp A vector indicating which  filename was uploaded to a temporal array in SciDB
#' @param isLoaded A vector indicating which  filename was uploaded to a permanent array in SciDB
#' @return A character vector
#' @docType methods
#' @export 
 setGeneric(name = "maskAsUploaded", def = function(object, modisFiles, isLoaded2tmp, isLoaded){standardGeneric("maskAsUploaded")})
 setMethod(
   f = "maskAsUploaded",
   signature = "ModisControl",
   definition = function(object, modisFiles, isLoaded2tmp, isLoaded){
     res <- .maskAsUploaded(modisFiles = modisFiles, isLoaded2tmp = isLoaded2tmp, isLoaded = isLoaded, user = getControlUser(object), password = getControlPassword(object), dbname = getControlDbname(object), host = getControlHost(object))
     return(res)
   }
 )








#*******************************************************
#WORKER
#*******************************************************

.maskAsUploaded <- function(user, password, dbname, host, modisFiles, isLoaded2tmp, isLoaded){
  
  res <- data.frame()
  if(length(modisFiles) > 0){
    ltmp <- is.null(isLoaded2tmp) == FALSE
    l <- is.null(isLoaded) == FALSE
    
    if(l == TRUE & ltmp == TRUE){
      sql <- sprintf( "('%s', %i, %i)", modisFiles, isLoaded2tmp, isLoaded)  
      sql <- paste(sql, collapse = ", ")
      sql <- paste("INSERT INTO  Image (idImage, loadedtmp, loaded) VALUES ", sql)
      sql <- paste(sql, ";")
    }else if(l == TRUE & ltmp == FALSE){
      sql <- sprintf( "('%s', %i)", modisFiles, isLoaded)  
      sql <- paste(sql, collapse = ", ")
      sql <- paste("INSERT INTO  Image (idImage, loaded) VALUES ", sql)
      sql <- paste(sql, ";")
    }else if (l == FALSE & ltmp == TRUE){
      sql <- sprintf( "('%s', %i)", modisFiles, isLoaded2tmp)  
      sql <- paste(sql, collapse = ", ")
      sql <- paste("INSERT INTO  Image (idImage, loadedtmp) VALUES ", sql)
      sql <- paste(sql, ";")
    }
    maxresults <- -1
    res <- .sendQuery(sql = sql, maxresults = maxresults, user = user, password = password, dbname = dbname, host = host, update = TRUE)
    
  }
  return(res)
}


# Checks is a MODIS image was already uploaded to SciDB
#
# @param modisFiles Character with the name of the MODIS hdf file
# @param user Database connection parameter
# @param password Database connection parameter
# @param dbname Database connection parameter
# @param host Database connection parameter
# @return A data.frame
.isUploaded <- function(modisFiles, user, password, dbname, host){
  #Builds a SQL query
  idImage <- rep("idImage = '", times = length(modisFiles))
  where <- paste(idImage, modisFiles, "'", sep ="")
  where <- paste(where, collapse = " OR ")
  sql <-  "SELECT * FROM Image WHERE "
  sql <- paste(sql, where)
  maxresults <- length(modisFiles)
  # Sends the query and retrieves the results
  res <- .sendQuery(sql = sql, maxresults = maxresults, user = user, password = password, dbname = dbname, host = host, update = FALSE)
  return(res)
}





#*******************************************************
#UTIL
#*******************************************************

# Sends a query to a database
#
# @param sql SQL query to be executed
# @param maxresults Maximum number of rows to retrieve
# @param user Database connection parameter
# @param password Database connection parameter
# @param dbname Database connection parameter
# @param host Database connection parameter
# @return A data.frame (empty when update == TRUE)
.sendQuery <- function(sql, maxresults, user, password, dbname, host, update){
  res <- data.frame()
  con <- dbConnect(MySQL(), user=user, password=password, dbname=dbname, host=host)
  qres <- dbSendQuery(con, sql)
  if(update != TRUE){
    res <- fetch(qres, n = maxresults)
    dbClearResult(qres)
  }
  on.exit(dbDisconnect(con))
  return(res)
}
  
