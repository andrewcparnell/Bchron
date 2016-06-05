\name{plot.BchronologyRun}
\alias{plot.BchronologyRun}
\title{
Plot output from Bchronology
}
\description{
Plots output from a run of \code{\link{Bchronology}}
}
\usage{
\method{plot}{BchronologyRun}(x, dateHeight = 30, \ldots)
}
\arguments{
  \item{x}{
The object created by \code{\link{Bchronology}}
}
  \item{dateHeight}{
The height of the date densities plotted in position/depth units
}
  \item{\dots}{
Other graphical parameters as detailed in \code{\link{par}}
}
}
\details{
Creates a simple plot of the chronology output. The height of the date densities in the plots can be manipulated via the \code{dateHeight} argument which is represented in the same units as the positions/depths provided. More detailed plots can be created by manipulating the Bchronology object as required.
}
\author{
Andrew Parnell <andrew.parnell@ucd.ie>
}
\seealso{
\code{\link{BchronCalibrate}}, \code{\link{Bchronology}} \code{\link{BchronRSL}}, \code{\link{BchronDensity}}, \code{\link{BchronDensityFast}} 
}
\examples{
# Examples in main function \code{\link{Bchronology}}
}
