\name{plot.BchronRSLRun}
\alias{plot.BchronRSLRun}
\title{
Plot output from BchronRSL
}
\description{
Plot output from the \code{\link{BchronRSL}} function
}
\usage{
\method{plot}{BchronRSLRun}(x, xlab = "Age (cal BP)", ylab = "Depth (m)", \ldots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
An object created by \code{\link{BchronRSL}}
}
  \item{xlab}{
(optional) Label for the x-axis of the plot
}
  \item{ylab}{
(optional) Label for the y-axis of the plot
}
  \item{\dots}{
Other arguments to plot, see \code{\link{par}}
}
}

\author{
Andrew Parnell <andrew.parnell@ucd.ie
}
\seealso{
\code{\link{BchronCalibrate}}, \code{\link{Bchronology}}, \code{\link{BchronRSL}}, \code{\link{BchronDensity}}, \code{\link{BchronDensityFast}} 
}
\examples{
# Examples in \code{\link{BchronRSL}}
}