\docType{methods}
\name{queryAfl}
\alias{queryAfl}
\title{Sends an AFL query to SciDB and return the results using scidbR}
\usage{
queryAfl(object, afl, iterative, ret)
}
\arguments{
  \item{object}{A ScidbInstance object}

  \item{afl}{AN AFL query}

  \item{iterative}{Bollean. Control if the response is an
  iterator. See scidbR::iquery}

  \item{ret}{Boolean. TRUE to return output. See
  scidb::iquery}
}
\value{
The results or an iterator (when iterative = TRUE). See
scidb::iquery
}
\description{
Sends an AFL query to SciDB and return the results using
scidbR
}

