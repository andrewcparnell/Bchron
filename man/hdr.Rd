\name{hdr}
\alias{hdr}
\title{
Calculate highest density regions for Bchron calibrated ages
}
\description{
A function for computing highest density regions (HDRs)
}
\usage{
hdr(date, prob = 0.95)
}
\arguments{
  \item{date}{
A calibrated Bchron date, via e.g. \code{\link{BchronCalibrate}}
}
  \item{prob}{
The desired probability interval, in the range(0, 1)
}
}
\details{
The output of this function is a list of contiguous ranges which cover the probability interval requested. A highest density region might have multiple such ranges if the calibrated date is multi-modal. These differ from credible intervals, which are always contiguous but will not be a good representation of a multi-modal probability distribution.
}
\author{
Andrew Parnell <andrew.parnell@ucd.ie>
}
\seealso{
\code{\link{BchronCalibrate}}
}
\examples{
# Calibrate multiple ages and summarise them
ages = BchronCalibrate(ages=11553,ageSds=230,
calCurves='intcal13')
# Get samples
hdr(ages$Date1)
}
