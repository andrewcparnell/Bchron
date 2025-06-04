#' Bchron: Radiocarbon dating, age-depth modelling, relative sea level rate estimation, and non-parametric phase modelling
#'
#' This package enables quick calibration of radiocarbon dates under various calibration curves (including user generated ones); Age-depth modelling as per the algorithm of Haslett and Parnell (2008); Relative sea level rate estimation incorporating time uncertainty in polynomial regression models; and non-parametric phase modelling via Gaussian mixtures as a means to determine the activity of a site (and as an alternative to the Oxcal function SUM)
#'
#' @section Bchron functions:
#' The most important functions are \code{\link{BchronCalibrate}} to calibrate radiocarbon (and non-radiocarbon) dates, \code{\link{Bchronology}} for the age-depth model of Haslett and Parnell (2008), \code{\link{BchronRSL}} to get rate estimates for relative sea level data, \code{\link{BchronDensity}} and \code{\link{BchronDensityFast}} for non-parametric phase modelling of age data. See the help files for these functions for examples. See the vignette for more complete documentation
#'
#' @name Bchron
"_PACKAGE"

utils::globalVariables(c("Age", "Density", "V1", "V2", "low", "high", "Density2",
                         "dens", "age", "calMid", "c14Mid", "c14Low", "c14High",
                         "calLow", "calHigh", "ageGrid", "densities", "height",
                         "Date", "RSL", "ageErr", "rslErr", "predLow", "predHigh",
                         "Rate", "rateLow", "rateHigh", "Acceleration", "accelLow",
                         "accelHigh", "Age", "Date", "densities", "height",
                         "positionLow", "positionHigh"))
