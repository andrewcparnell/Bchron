\name{predict.BchronologyRun}
\alias{predict.BchronologyRun}
\title{
Predict ages of other positions for a BchronologyRun object
}
\description{
This function will predict the ages of new positions (usually depths) based on a previous run of the function \code{\link{Bchronology}}. It will also allow for thickness uncertainties to be included in the resulting ages, for example when the age of a particular event is desired
}
\usage{
\method{predict}{BchronologyRun}(object, newPositions, newPositionThicknesses = NULL, maxExtrap = 500, ...)
}
\arguments{
  \item{object}{
Output from a run of \code{\link{Bchronology}}
}
  \item{newPositions}{
A vector of new positions at which to find ages
}
  \item{newPositionThicknesses}{
A vector of thicknesses for the above positions. Must be the same length as \code{newPositions}
}
  \item{maxExtrap}{
The maximum new of extrapolation attempts. It might be worth increasing this if you are extrapolating a long way from the other dated positions
}
  \item{\dots}{
Other arguments to predict (not currently supported)
}
}
\author{
Andrew Parnell <andrew.parnell@ucd.ie
}
\seealso{
\code{\link{BchronCalibrate}}, \code{\link{Bchronology}} \code{\link{BchronRSL}}, \code{\link{BchronDensity}}, \code{\link{BchronDensityFast}} 
}
\examples{
# Examples in main function \code{\link{Bchronology}}
}
