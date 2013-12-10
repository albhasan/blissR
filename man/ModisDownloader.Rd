\name{ModisDownloader}
\alias{ModisDownloader}
\alias{ModisDownloader-class}
\title{The MODISDOWNLOADER class}
\description{
  Use this class for downloading MODIS tiles.
}
\details{
  You can specify MODIS's products, tiles, collections,
  satellites, and time interval.
}
\note{
  No notes
}
\section{Slots}{
  \describe{ \item{\code{timeWindowStart}:}{Object of class
  \code{"character"}, it is the start date of the time
  interval for selecting MODIS data}
  \item{\code{timeWindowEnd}:}{Object of class
  \code{"character"}, it is the end date of the time
  interval for selecting MODIS data}
  \item{\code{requestedTiles}:}{Object of class
  \code{"character"}, it is whitespace separated list of
  MODIS tiles to download}
  \item{\code{requestedProducts}:}{Object of class
  \code{"character"}, it is whitespace separated list of
  MODIS products to download}
  \item{\code{collections}:}{Object of class
  \code{"character"}, it is whitespace separated list of
  MODIS collections to download}
  \item{\code{baseUrl}:}{Object of class
  \code{"character"}, it is the URL for searching the MODIS
  tiles. For dowloading the tiles the MODIS package is
  used} \item{\code{localArcPath}:}{Object of class
  \code{"character"}, it is the path to the folder for
  storing the MODIS files. It is the path to the
  "MODIS_ARC" (usually it contains a folder called
  "PROCESSED"). See the MODIS package documentation -
  MODISoptions} }
}
\author{
  Alber Sanchez
}

