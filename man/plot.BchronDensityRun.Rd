\name{plot.BchronDensityRun}
\alias{plot.BchronDensityRun}
\title{
Plot output from BchronDensity
}
\description{
A plot of the output from \code{\link{BchronDensity}}
}
\usage{
\method{plot}{BchronDensityRun}(x,
plotDates = TRUE,
plotSum = FALSE,
plotPhase = TRUE,
phaseProb = 0.95,
\ldots)
}
\arguments{
  \item{x}{
Output from \code{\link{BchronDensity}}
}
  \item{plotDates}{
Whether to plot the individual calibrated dates (default TRUE)
}
  \item{plotSum}{
Whether to plot the sum of the probability distributions (default FALSE)
}
  \item{plotPhase}{
Whether to plot the phase values
}
  \item{phaseProb}{
The probability value for the phase identification
}
  \item{\dots}{
Other graphical commands. See \code{\link{par}}
}
}
\author{
Andrew Parnell <andrew.parnell@ucd.ie>
}
\seealso{
\code{\link{Bchronology}}, \code{\link{BchronRSL}}, \code{\link{BchronDensity}}, \code{\link{BchronDensityFast}} for a faster approximate version of this function
}
\examples{
# Examples in \code{\link{BchronDensity}}
}