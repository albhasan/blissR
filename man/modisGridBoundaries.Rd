\name{modisGridBoundaries}
\alias{modisGridBoundaries}
\docType{data}
\title{
MODIS tiles' boundaries.
}
\description{
This file contains the boundaries of the MODIS' tiles.
}
\usage{data(modisGridBoundaries)}
\format{
  A data.frame
  \describe{
    \item{\code{iv}}{An integer representing a tile's vertical index}
    \item{\code{ih}}{An integer representing a tile's vertical index}
    \item{\code{lon_min}}{A double representing a tile's minimum longitude in the MODIs sinusoidal projection}
    \item{\code{lon_max}}{A double representing a tile's maximum longitude in the MODIs sinusoidal projection}
    \item{\code{lat_min}}{A double representing a tile's minimum latitude in the MODIs sinusoidal projection}
    \item{\code{lat_max}}{A double representing a tile's maximum latitude in the MODIs sinusoidal projection}
  }
}
%%\details{
%%  ~~ If necessary, more details than the __description__ above ~~
%%}
\source{
File taken from http://landweb.nascom.nasa.gov/developers/is_tiles/is_bound_10deg.txt
}
\references{
MODIS Grids http://modis-land.gsfc.nasa.gov/MODLAND_grid.html
}
\examples{
data(modisGridBoundaries)
}
\keyword{datasets}