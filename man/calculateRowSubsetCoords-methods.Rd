\docType{methods}
\name{calculateRowSubsetCoords}
\alias{calculateRowSubsetCoords}
\title{Calculates the coordinates for a subset of rows of a raster}
\usage{
calculateRowSubsetCoords(object, xRes, yRes, xmin, ymax, ncols, nrows, startRow)
}
\arguments{
  \item{object}{An object of the type ModisGrid}

  \item{xRes}{Image resolution in the x direction}

  \item{yRes}{Image resolution in the y direction}

  \item{xmin}{Minimum x coordinate of the whole image}

  \item{ymax}{Maximum y coordinate of the whole image}

  \item{ncols}{Total number of columns in the image}

  \item{nrows}{Number of consecutive rows taken from the
  image}

  \item{startRow}{Index of the first row in the row subset}
}
\value{
A list containing 2 numeric vectors with the x and y
coordinates
}
\description{
Calculates the coordinates for a subset of rows of a raster
}

