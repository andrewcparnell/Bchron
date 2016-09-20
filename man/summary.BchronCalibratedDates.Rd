\name{summary.BchronCalibratedDates}
\alias{summary.BchronCalibratedDates}
\title{
Summarise a BchronCalibrate object
}
\description{
Produces summary output from a \code{\link{BchronCalibrate}} run, including the highest density regions for the calibrated ages for given probability levels
}
\usage{
\method{summary}{BchronCalibratedDates}(object, prob = 95, \ldots, digits = max(3, getOption("digits") - 3))
}
\arguments{
  \item{object}{
The output of a run of \code{\link{BchronCalibrate}}
}
  \item{prob}{
A percentage value (between 0 and 100) at which the highest density regions for each age are calculated
}
  \item{\dots}{
Further arguments (not currently supported)
}
  \item{digits}{
Significant digits to display (not currently supported)
}
}
\references{
Forthcoming!
}
\author{
Andrew Parnell <andrew.parnell@ucd.ie>
}
\seealso{
\code{\link{BchronCalibrate}}, \code{\link{Bchronology}}, \code{\link{BchronRSL}}, \code{\link{BchronDensity}}, \code{\link{BchronDensityFast}}
}
\examples{
# Examples in BchronCalibrate and the vignette
}
