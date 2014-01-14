#' The CONSTANT class
#'
#' Use this class for keeping constants
#'
#'
#'@section Slots :
#'  \describe{
#'    \item{\code{path2scidbBin}:}{Object of class \code{"character"}, Path to the bin folder in the SciDB's coordinator.}
#'  }
#'
#' @note No notes
#' @name Constant
#' @aliases Constant-class
#' @exportClass Constant
#' @author Alber Sanchez
setClass(
  Class = "Constant", 
  slots = c(path2scidbBin = "character"),
  validity = function(object){
    #cat("~~~ Constant: inspector ~~~ \n")
    res <- TRUE
    if(nchar(object@path2scidbBin) < 3)
      res <- FALSE
    if(res == FALSE)
      stop ("[Constant: validation] Some parameters are invalid")
    return(res)
  }
)

#*******************************************************
#CONSTRUCTOR
#*******************************************************
setMethod(
  f="initialize",
  signature="Constant",
  definition=function(.Object, path2scidbBin = "/opt/scidb/13.12/bin/"){
    #cat ("~~~~~ Constant: initializator ~~~~~ \n")
    .Object@path2scidbBin <- path2scidbBin
    validObject(.Object)# call of the inspector
    return(.Object)
  }
)

#*******************************************************
#ACCESSORS
#*******************************************************

#' Returns the object host
#' 
#' @param object A Constant object
#' @docType methods
#' @export 
setGeneric("getPath2scidbBin",function(object){standardGeneric ("getPath2scidbBin")})
setMethod("getPath2scidbBin","Constant",
          
          function(object){
            return(object@path2scidbBin)
          }
)
