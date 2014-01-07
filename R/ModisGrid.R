#' The MODIS GRID class
#'
#' This class contains code for handling spatio-temporal convertions between SciDB arrays indexes and other reference systems 
#'
#'
#' @note No notes
#' @name ModisGrid
#' @aliases ModisGrid-class
#' @exportClass ModisGrid
#' @author Alber Sanchez
setClass(
  Class = "ModisGrid", 
  slots = c(dummy = "character"),
  validity = function(object){
    #cat("~~~ ModisGrid: inspector ~~~ \n")
    res <- TRUE
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
  definition=function(.Object, dummy = ""){
    #cat ("~~~~~ ModisGrid: initializator ~~~~~ \n")
    .Object@dummy <- dummy
    validObject(.Object)# call of the inspector
    return(.Object)
  }
)
#*******************************************************
#ACCESSORS
#*******************************************************

#*******************************************************
#GENERIC METHODS
#*******************************************************

#*******************************************************
#METHODS
#*******************************************************