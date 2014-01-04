\name{getFirstGmip}
\alias{getFirstGmip}
\title{Returns the GMPI of the first pixel (top left) of the given MODIS tile}
\usage{
getFirstGmip(object, modisTileId, nrows, ncols)
}
\arguments{
  \item{object}{An instance of the class Util}

  \item{modisTileId}{A character with a MODIS tile id (i.e
  "h10v08")}

  \item{nrows}{Number of rows in a MODIS image (i.e for
  MOD09Q1 is 4800)}

  \item{ncols}{Number of rows in a MODIS image (i.e for
  MOD09Q1 is 4800)}
}
\value{
Numeric vector containing the c(i,j) pixel coordinates in
th GMPI
}
\description{
Returns the GMPI of the first pixel (top left) of the given
MODIS tile
}

