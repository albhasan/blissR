\name{downloadRegionHdfs}
\alias{downloadRegionHdfs}
\title{Downloads the MODIS MOD09Q1 corresponding to the amazon (Tiles H <- 10:13; V <- 8:10)}
\usage{
  downloadRegionHdfs(region, fromDate, toDate)
}
\arguments{
  \item{region}{Region name. One of "amazon",
  "southamerica"}

  \item{fromDate}{Start date}

  \item{toDate}{End date}
}
\value{
  Vector of the paths to the downloaded files
}
\description{
  Downloads the MODIS MOD09Q1 corresponding to the amazon
  (Tiles H <- 10:13; V <- 8:10)
}

