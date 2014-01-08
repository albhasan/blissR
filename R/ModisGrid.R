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
  slots = c(spaceResolution = "numeric",
            timeOrigin = "character",
            timeResolution = "numeric"),#IN DAYS 
  validity = function(object){
    #cat("~~~ ModisGrid: inspector ~~~ \n")
    res <- TRUE
    if(object@spaceResolution < 0)
      res <- FALSE
    if(nchar(object@timeOrigin) < 0)
      res <- FALSE
    if(object@timeResolution < 0)
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
  definition=function(.Object, spaceResolution, timeOrigin, timeResolution){
    #cat ("~~~~~ ModisGrid: initializator ~~~~~ \n")
    .Object@spaceResolution <- spaceResolution
    .Object@timeOrigin <- timeOrigin
    .Object@timeResolution <- timeResolution
    validObject(.Object)# call of the inspector
    return(.Object)
  }
)
#*******************************************************
#ACCESSORS
#*******************************************************


#' Returns the object spatial resolution
#' 
#' @param object A ModisGrid object
#' @docType methods
#' @export 
setGeneric("getSpaceResolution",function(object){standardGeneric ("getSpaceResolution")})
setMethod("getSpaceResolution","ModisGrid",
          
          function(object){
            return(object@spaceResolution)
          }
)


#' Returns the origin of the time grid
#' 
#' @param object A ModisGrid object
#' @docType methods
#' @export 
setGeneric("getTimeOrigin",function(object){standardGeneric ("getTimeOrigin")})
setMethod("getTimeOrigin","ModisGrid",
          
          function(object){
            return(object@timeOrigin)
          }
)


#' Returns the time among observations in the grid
#' 
#' @param object A ModisGrid object
#' @docType methods
#' @export 
setGeneric("getTimeResolution",function(object){standardGeneric ("getTimeResolution")})
setMethod("getTimeResolution","ModisGrid",
          
          function(object){
            return(object@timeResolution)
          }
)




#*******************************************************
#GENERIC METHODS
#*******************************************************

#*******************************************************
#METHODS
#*******************************************************

#' Converts a date into grid time index
#'
#' @param object Object of the class ModisGrid
#' @param dateAsText Date as a text string
#' @return A date object (POSIXlt)
#' @docType methods
#' @export 
setGeneric(name = "date2grid", def = function(object, dateAsText){standardGeneric("date2grid")})
setMethod(
  f = "date2grid",
  signature = "ModisGrid",
  definition = function(object, dateAsText){
    
    timeOrigin <- getTimeOrigin(object)
    timeResolution <- getTimeResolution(object)
    res <- .date2grid(dateAsText = dateAsText, timeOrigin = timeOrigin, timeResolution = timeResolution)
    return(res)
  }
)


#*******************************************************
#WORKER
#*******************************************************


# Converts a date into grid time units
#
# @param dateAsText Date as a text string
# @return A date object (POSIXlt)
.date2grid <- function(dateAsText, timeOrigin, timeResolution){
  u <- new("Util")
  gridOrigin <- text2date(u, dateAsText = timeOrigin)
  inputDate <- text2date(u, dateAsText = dateAsText)
  inputDateDoy <- as.numeric(substrRight(u, date2ydoy(u, dateAsText = dateAsText), n = 3))
  y0 <- as.numeric(format(as.Date(gridOrigin), "%Y"))
  y1 <- as.numeric(format(as.Date(inputDate), "%Y"))
  ind <- seq(from = 1, to = 366, by = timeResolution)
  if(inputDateDoy %in% ind){
    res <- ((y1 - y0) * length(ind)) + which(ind == inputDateDoy) -1
  }else{
    res <- NA
  }
  
  
  
#   if(timeResolutionUnit == "days"){
#     unitsAyear <- round(365 / 8)  
#   }
#   yearOriginText <- paste(format(d, "%Y"), "/01/01", sep="")# MODIS origin is January the first of each year
#   yearOrigin <- as.POSIXlt(yearOriginText)
#   daydif <- as.numeric(as.Date(d) - as.Date(yearOrigin))#as.numeric(difftime(time1 = d, time2 = gOrigin, units = timeResolutionUnit))
#   y0 <- as.numeric(format(as.Date(gridOrigin), "%Y"))
#   y1 <- as.numeric(format(as.Date(yearOrigin), "%Y"))
#   if(daydif %% timeResolution == 0){
#     res <- ((y1 - y0) * unitsAyear) + (daydif / timeResolution) 
#   }else{
#     res <- NA
#   }
  return(res)
}

# Converts a grid time units into a date
.grid2date <- function(gridDateIndex, timeOrigin, timeResolution){
  u <- new("Util")
  d <- text2date(u, dateAsText = gridDateIndex)
  originText <- paste(format(d, "%Y"), "/01/01", sep="")# MODIS origin is January the first of each year
  
}





.buildGrid <- function(){
  gt <- GridTopology(c(0,0,0), c(1,1,1), c(5,4,3))
  
}




