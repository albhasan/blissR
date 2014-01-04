\name{modisGridBoundaries}
\alias{modisGridBoundaries}
\docType{data}
\title{
MODIS Grids
}
\description{
There are 460 non-fill tiles, tiles are 10 degrees by 10 degrees at the equator. The tile coordinate system starts at (0,0) (horizontal tile number, vertical tile number) in the upper left corner and proceeds right (horizontal) and downward (vertical). The tile in the bottom right corner is (35,17).
}
\usage{data(modisGridBoundaries)}
\format{
  A data frame with 648 observations on the following 6 variables.
  \describe{
    \item{\code{iv}}{a numeric vector. It represents the row index}
    \item{\code{ih}}{a numeric vector. It represents the column index}
    \item{\code{lon_min}}{a numeric vector. Minimum longitude}
    \item{\code{lon_max}}{a numeric vector. Maximum longitude}
    \item{\code{lat_min}}{a numeric vector. Minimum latitude}
    \item{\code{lat_max}}{a numeric vector. Maximum latitude}
  }
}
\source{
http://modis-land.gsfc.nasa.gov/pdf/sn_bound_10deg.txt
}
\examples{
data(modisGridBoundaries)
}
\keyword{datasets, MODIS}