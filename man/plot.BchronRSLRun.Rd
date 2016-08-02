\name{plot.BchronRSLRun}
\alias{plot.BchronRSLRun}
\title{
Plot output from BchronRSL
}
\description{
Plot output from the \code{\link{BchronRSL}} function
}
\usage{
\method{plot}{BchronRSLRun}(x, type = c('RSL','rate','accel'), \ldots)
}
\arguments{
  \item{x}{
An object created by \code{\link{BchronRSL}}
}
  \item{type}{
One of 'RSL', 'rate', or 'accel'. If RSL produces a plot of RSL esimtates from the model. If rate, produces rate estimates. If accel produces acceleration estimates.
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
# Examples in BchronRSL and vignette
}