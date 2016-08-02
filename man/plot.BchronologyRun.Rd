\name{plot.BchronologyRun}
\alias{plot.BchronologyRun}
\title{
Plot output from Bchronology
}
\description{
Plots output from a run of \code{\link{Bchronology}}
}
\usage{
\method{plot}{BchronologyRun}(x, dateHeight = 30,
chronCol = grDevices::rgb(190/255,190/255,190/255,alpha=0.8),
chronBorder = grDevices::rgb(190/255,190/255,190/255,alpha=0.8),
alpha = 0.95,\ldots)
}
\arguments{
  \item{x}{
The object created by \code{\link{Bchronology}}
}
  \item{dateHeight}{
The height of the date densities plotted in position/depth units
}
  \item{chronCol}{
The colour of the chronology uncertainty ribbon to be plotted
}
  \item{chronBorder}{
The colour of the border of the chronology uncertainty ribbon to be plotted
}
  \item{alpha}{
The credible interval of the chronology run to be plotted. Defaults to 95 percent
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
