#' The MODISLOADER class
#'
#' Use this class for loading to SciDB processed MODIS tiles
#'
#' You can ???????
#'
#'@section Slots :
#'  \describe{
#'    \item{\code{files}:}{Object of class \code{"character"}, it is a vector with the paths to the files.}
#'  }
#'
#' @note No notes
#' @name ModisLoader
#' @aliases ModisLoader-class
#' @exportClass ModisLoader
#' @author Alber Sanchez
setClass(
  Class = "ModisLoader", 
  slots = c(files = "character"),
  validity = function(object){
    #cat("~~~ ModisProcessor: inspector ~~~ \n")
    res <- TRUE
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
  definition=function(.Object, files){
    #cat ("~~~~~ ModisLoader: initializator ~~~~~ \n")
    .Object@files <- files
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

#*******************************************************
#GENERIC METHODS
#*******************************************************

#*******************************************************
#METHODS
#*******************************************************

#' Loads the input files to SciDB and it deletes the source file
#' 
#' @param object A ModisLoader object
#' @return A character vector
#' @docType methods
#' @export 
setGeneric(name = "load", def = function(object){standardGeneric("load")})
setMethod(
  f = "load",
  signature = "ModisProcessor",
  definition = function(object){
    # @param object A ModisProcessor object
    res <- .load(files = getFiles(object))
    return(res)
  }
)


#*******************************************************
#WORKER
#*******************************************************

# Loads the input files
# 
# @return A character vector
.load <- function(){
  f <- getFiles(object)
  res <- vector(mode = "character", length = length(f))
  for(i in 1:(length(f))){
    file <- f[i]
    base1 <- "loadcsv.py -n 1 -t NNNN -a '"
    base2 <- "' -s '<i:int64, j:int64, t:int64, value:double> [k=0:*,1000000,0]' -i '"
    base3 <- "'-x -A '"
    base4 <- "'"
    
    if(substrRight(file, 7) == "b01.txt"){
      tmpArray <- "loadMOD09Q1sur_refl_b01"
      destArray <- "MOD09Q1sur_refl_b01"
    }else if(substrRight(file, 7) == "b02.txt"){
      tmpArray <- "loadMOD09Q1sur_refl_b02"
      destArray <- "MOD09Q1sur_refl_b02"
    }else if(substrRight(file, 16) == "refl_qc_250m.txt"){
      tmpArray <- "loadMOD09Q1sur_refl_qc_250m"
      destArray <- "MOD09Q1sur_refl_qc_250m"
    }
    cmd <- paste0(base1, tmpArray, base2, file, base3, destArray)
    system(cmd)
    system(paste0("rm -f ", file))
    res[i] <- ""
  }
  return(res)
}

#*******************************************************
#UTIL
#*******************************************************

# Returns the right characters of a string - taken from http://stackoverflow.com/questions/7963898/extracting-the-last-n-characters-from-a-string-in-r
#
# @param x A string
# @param n Number of characters to return
# @return A string
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
