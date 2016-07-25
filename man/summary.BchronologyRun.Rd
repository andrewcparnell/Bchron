\name{summary.BchronologyRun}
\alias{summary.BchronologyRun}

\title{
Summarise a Bchronology object
}
\description{
Summarise a \code{\link{Bchronology}} object
}
\usage{
\method{summary}{BchronologyRun}(object, type=c('quantiles','outliers','convergence'),
probs = c(0.025, 0.1, 0.5, 0.9, 0.975), \ldots, digits = max(3, getOption("digits") - 3), useExisting = TRUE)
}
\arguments{
  \item{object}{
Output from a run of \code{\link{Bchronology}}
}
  \item{type}{
(optional) Type of output required. The default (quantiles) gives the quantiles of the ages for each position in \code{predictPositions} from \code{\link{Bchronology}}. The other options provide outlier probabilities or convergence diagnostics.
}
  \item{probs}{
(optional) Probabilities (between 0 and 1) at which to summarise the predicted chronologies
}
  \item{\dots}{
Other arguments (not currently supported)
}
  \item{digits}{
Number of digits to report values
}
  \item{useExisting}{
Whether to use the predicted chronologies/positions to calculate the sedimentation rate (if TRUE - default) or to re-create them based on a unit-scaled position grid (if FALSE). The latter will be a little bit slower but will provide better sedimentation rate estimates if the original positions are not on a unit scale (e.g. each cm)
}

}
\author{
Andrew Parnell <andrew.parnell@ucd.ie
}
\seealso{
\code{\link{BchronCalibrate}}, \code{\link{Bchronology}} \code{\link{BchronRSL}}, \code{\link{BchronDensity}}, \code{\link{BchronDensityFast}}
}
\examples{
# Examples in main function Bchronology and in the vignette
}
