\name{Bchron-package}
\alias{Bchron-package}
\alias{Bchron}
\docType{package}
\title{
Radiocarbon dating, age-depth modelling, relative sea level rate estimation, and non-parametric phase modelling
}
\description{
This package enables quick calibration of radiocarbon dates under various calibration curves (including user generated ones); Age-depth modelling as per the algorithm of Haslett and Parnell (2008); Relative sea level rate estimation incorporating time uncertainty in polynomial regression models; and non-parametric phase modelling via Gaussian mixtures as a means to determine the activity of a site (and as an alternative to the Oxcal function SUM).}
\details{
\tabular{ll}{
Package: \tab Bchron\cr
Type: \tab Package\cr
Version: \tab 4.0\cr
Date: \tab 2014-03-26\cr
License: \tab GPL (>= 2)
}
Most important functions are \code{\link{BchronCalibrate}} to calibrate radiocarbon (and non-radiocarbon) dates, \code{\link{Bchronology}} for the age-depth model of Haslett and Parnell (2008), \code{\link{BchronRSL}} to get rate estimates for relative sea level data, \code{\link{BchronDensity}} and \code{\link{BchronDensityFast}} for non-parametric phase modelling of age data. See the help files for these functions for examples
}
\author{
Andrew Parnell <andrew.parnell@ucd.ie>
}
\references{
See individual functions for references and examples
}
