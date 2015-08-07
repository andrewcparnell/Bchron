\name{plot.BchronDensityRunFast}
\alias{plot.BchronDensityRunFast}
\title{
Plot run from BchronDensityFast
}
\description{
Plots output from \code{\link{BchronDensityFast}}
}
\usage{
\method{plot}{BchronDensityRunFast}(x, plotDates = TRUE, plotSum = FALSE, \ldots)
}
\arguments{
  \item{x}{
Output from \code{\link{BchronDensityFast}}
}
  \item{plotDates}{
(optional) Whether to include individual age pdfs (default TRUE)
}
  \item{plotSum}{
(optional) Whether to include sum of age pdfs (default FALSE)
}
  \item{\dots}{
Other graphical parameters, see \code{\link{par}}
}
}
\details{
Creates a basic plot of output for a run of \code{\link{BchronDensityFast}}
}
\author{
Andrew Parnell <andrew.parnell@ucd.ie>
}
\seealso{
\code{\link{Bchronology}}, \code{\link{BchronRSL}}, \code{\link{BchronDensity}}, \code{\link{BchronDensityFast}} for a faster approximate version of this function
}
\examples{
# Examples in \code{\link{BchronDensityFast}}
}