\docType{methods}
\name{maskAsUploaded}
\alias{maskAsUploaded}
\title{Marks the given MODIS files as uploaded}
\usage{
maskAsUploaded(object, modisFiles, isLoaded2tmp, isLoaded)
}
\arguments{
  \item{object}{A ModisLoader object}

  \item{modisFiles}{A vector of MODIS HDF filenames}

  \item{isLoaded2tmp}{A vector indicating which filename
  was uploaded to a temporal array in SciDB}

  \item{isLoaded}{A vector indicating which filename was
  uploaded to a permanent array in SciDB}
}
\value{
A character vector
}
\description{
Marks the given MODIS files as uploaded
}

