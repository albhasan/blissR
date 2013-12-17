\name{calculateRowSubsetPixelCoords}
\alias{calculateRowSubsetPixelCoords}
\title{Calculates the pixel coordinates for a subset of rows of a raster}
\usage{
calculateRowSubsetPixelCoords(object, nrows, ncols, nsubsetrows, startRow)
}
\arguments{
  \item{object}{An ModisGrid object}

  \item{nrows}{Total number of rows in the image}

  \item{ncols}{Total number of columns in the image}

  \item{nsubsetrows}{Number of consecutive rows taken from
  the image}

  \item{startRow}{Index of the first row in the row subset}
}
\value{
A list containing 2 numeric vectors with the pixel's x and
y coordinates
}
\description{
Calculates the pixel coordinates for a subset of rows of a
raster
}

