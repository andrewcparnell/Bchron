\name{summary.BchronRSLRun}
\alias{summary.BchronRSLRun}

\title{
Summarise a BchronRSL run
}
\description{
Summarise a \code{\link{BchronRSL}} run
}
\usage{
\method{summary}{BchronRSLRun}(object, type = c('parameters', 'RSL', 'rate', 'accel'), age_grid = NULL, ...)
}
\arguments{
  \item{object}{
The output from a run of \code{\link{BchronRSL}}
}
  \item{type}{
One of 'parameters', 'RSL', 'rate', or 'accel'. If parameters, provides posterior credibility intervals of the regression coefficients. If RSL provides predicted RSL values. If rate, provides rate estimates. If accel provides acceleration estimates.
}
  \item{age_grid}{
An optional age grid for computing RSL, rate, or acceleration estimates. If not provided uses the age range of the Bchronology run.
}
  \item{\dots}{
(optional) Other arugments to functions (not currently implemeted)
}
}
\author{
Andrew Parnell <andrew.parnell@ucd.ie>
}
\seealso{
\code{\link{BchronCalibrate}}, \code{\link{Bchronology}}, \code{\link{BchronRSL}}, \code{\link{BchronDensity}}, \code{\link{BchronDensityFast}}
}
\examples{
# Examples in \code{\link{BchronRSL}}
}