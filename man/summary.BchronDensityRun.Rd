\name{summary.BchronDensityRun}
\alias{summary.BchronDensityRun}
\title{
Summarise a Bchron density object
}
\description{
Summarise a \code{\link{BchronDensity}} object
}
\usage{
\method{summary}{BchronDensityRun}(object,prob=0.95, \ldots, digits = max(3, getOption("digits")-3))
}
\arguments{
  \item{object}{
Output from a run of \code{\link{BchronDensity}}
}
  \item{prob}{
Probability for identifying phases
}
  \item{\dots}{
Other arguments (not currently supported)
}
  \item{digits}{
Number of digits to report values
}
}
\author{
Andrew Parnell <andrew.parnell@ucd.ie
}
\seealso{
\code{\link{BchronDensity}}
}
\examples{
# Examples in main function BchronDensity and in the vignette
}
