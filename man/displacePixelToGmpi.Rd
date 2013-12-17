\name{displacePixelToGmpi}
\alias{displacePixelToGmpi}
\title{Takes pixel coordinates and displace them to fit the Global Modis Index}
\usage{
displacePixelToGmpi(object, modisTileId, nrows, ncols, iLocalCoords,
  jLocalCoords)
}
\arguments{
  \item{object}{An ModisGrid object}

  \item{modisTileId}{A character with a MODIS tile id (i.e
  "h10v08")}

  \item{nrows}{Number of rows in a MODIS image (i.e for
  MOD09Q1 is 4800)}

  \item{ncols}{Number of rows in a MODIS image (i.e for
  MOD09Q1 is 4800)}

  \item{iLocalCoords}{Numeric coords of the pixel ids local
  to the image}

  \item{jLocalCoords}{Numeric coords of the pixel ids local
  to the image}
}
\value{
A list with 2 elements: The resulting i and j coords
}
\description{
Takes pixel coordinates and displace them to fit the Global
Modis Index
}

