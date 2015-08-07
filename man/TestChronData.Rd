\name{TestChronData}
\alias{TestChronData}
\docType{data}
\title{
Example chronology file for use with the BchronRSL function.
}
\description{
Some example chronology data for use with the \code{\link{BchronRSL}} function
}
\usage{data(Glendalough)}
\format{
  A data frame with 27 observations on the following 6 variables.
  \describe{
    \item{\code{id}}{ID names}
    \item{\code{ages}}{Ages in years BP}
    \item{\code{ageSds}}{Ages standard deviations in years BP}
    \item{\code{position}}{Depths in cm}
    \item{\code{thickness}}{Thicknesses in cm}
    \item{\code{calCurves}}{Calibration curve for each age}
  }
}
\references{
Andrew C. Parnell and W. Roland Gehrels (2013) 'Using chronological models in late holocene sea level reconstructions from salt marsh sediments' In: I. Shennan, B.P. Horton, and A.J. Long (eds). Handbook of Sea Level Research. Chichester: Wiley
}
\examples{
data(TestChronData)
# See \code{\link{BchronRSL}}
}
\keyword{datasets}
