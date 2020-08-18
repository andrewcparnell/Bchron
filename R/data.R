#' Glendalough data
#'
#' Chronology data for Glendalough data set
#'
#' @format A data frame with 6 observations on the following 6 variables:
#' \describe{
#'   \item{\code{id}}{ID of each age}
#'   \item{\code{ages}}{Age in (14C) years BP}
#'   \item{\code{ageSds}}{Age standard deviations}
#'   \item{\code{position}}{Depths in cm}
#'   \item{\code{thickness}}{Thicknesses in cm}
#'   \item{\code{calCurves}}{Calibration curve for each age}
#' }
#' 
#' @details This Glendalough data can be used with \code{\link{Bchronology}} or \code{\link{BchronDensity}}
#' @usage data(Glendalough)
#' 
#' @source Haslett, J., Whiley, M., Bhattacharya, S., Mitchell, F. J. G., Allen, J. R. M., Huntley, B., \& Salter-Townshend, M. (2006). Bayesian palaeoclimate reconstruction. Journal of the Royal Statistical Society, Series A, 169, 395-438.
"Glendalough"


#' Northern hemisphere 2013 calibration curve
#'
#' Northern hemisphere 2013 calibration curve
#'
#' @format A data frame with 5141 observations on 5 variables.
#' @usage data(intcal13)
#' 
#' @details For full details and reference see http://intcal.org/blurb.html. For usage details see \code{\link{BchronCalibrate}}
"intcal13"

#' Northern hemisphere 2020 calibration curve
#'
#' Northern hemisphere 2020 calibration curve
#'
#' @format A data frame with 9501 observations on 5 variables.
#' @usage data(intcal20)
#' 
#' @details For full details and reference see http://intcal.org/blurb.html. For usage details see \code{\link{BchronCalibrate}}
"intcal20"


#' Marine 2013 calibration curve
#'
#' Marine 2013 calibration curve
#'
#' @format A data frame with 4801 observations on 5 variables
#' @usage data(marine13)
#' 
#' @details For full details and reference see http://intcal.org/blurb.html. For usage details see \code{\link{BchronCalibrate}}
"marine13"

#' Marine 2020 calibration curve
#'
#' Marine 2020 calibration curve
#'
#' @format A data frame with 5501 observations on 5 variables
#' @usage data(marine20)
#' 
#' @details For full details and reference see http://intcal.org/blurb.html. For usage details see \code{\link{BchronCalibrate}}
"marine20"

#' Data for dummy calibration of normally distributed ages
#'
#' Data for dummy calibration of normally distributed ages
#'
#' @format A data frame with 2 observations on 3 variables.
#' @usage data(normal)
#' 
#' @details This is dummy data so that \code{\link{BchronCalibrate}} can calibrate normally distributed dates.
"normal"


#' Southern hemisphere 2013 calibration curve
#'
#' Southern hemisphere 2013 calibration curve
#'
#' @format A data frame with 5141 observations on 5 variables.
#' @usage data(shcal13)
#' 
#' @details For full details and reference see http://intcal.org/blurb.html. For usage details see \code{\link{BchronCalibrate}}
"shcal13"

#' Southern hemisphere 2020 calibration curve
#'
#' Southern hemisphere 2020 calibration curve
#'
#' @format A data frame with 9501 observations on 5 variables.
#' @usage data(shcal20)
#' 
#' @details For full details and reference see http://intcal.org/blurb.html. For usage details see \code{\link{BchronCalibrate}}
"shcal20"

#' Sluggan Moss data
#'
#' Chronology data for Sluggan Moss data set
#'
#' @format A data frame with 31 observations on the following 6 variables:
#' \describe{
#' \item{\code{id}}{ID of each age}
#' \item{\code{ages}}{Age in (14C) years BP}
#' \item{\code{ageSds}}{Age standard deviations}
#' \item{\code{position}}{Depths in cm}
#' \item{\code{thickness}}{Thicknesses in cm}
#' \item{\code{calCurves}}{Calibration curve for each age}
#' }
#' 
#' @details This Sluggan Moss data can be downloaded from the European Pollen Database: \url{http://www.europeanpollendatabase.net}. For usage see \code{\link{Bchronology}} or \code{\link{BchronDensity}}
#' @usage data(Sluggan)
#'
#' @source Smith, A. G., \& Goddard, I. C. (1991). A 12,500 year record of vegetational history at Sluggan Bog, Co. Antrim, N. Ireland (incorporating a pollen zone scheme for the non-specialist). New Phytologist, 118, 167-187.
"Sluggan"


#' Example chronology file for use with the BchronRSL function.
#'
#' Some example chronology data for use with the \code{\link{BchronRSL}} function
#'
#' @format A data frame with 27 observations on the following 6 variables:
#' \describe{
#' \item{\code{id}}{ID names}
#' \item{\code{ages}}{Ages in years BP}
#' \item{\code{ageSds}}{Ages standard deviations in years BP}
#' \item{\code{position}}{Depths in cm}
#' \item{\code{thickness}}{Thicknesses in cm}
#' \item{\code{calCurves}}{Calibration curve for each age}
#' }
#' 
#' @usage data(TestChronData)
#' @source Andrew C. Parnell and W. Roland Gehrels (2013) 'Using chronological models in late holocene sea level reconstructions from salt marsh sediments' In: I. Shennan, B.P. Horton, and A.J. Long (eds). Handbook of Sea Level Research. Chichester: Wiley
"TestChronData"

#' Relative sea level data
#'
#' A set of relative sea level data for use with \code{\link{BchronRSL}}
#'
#' @format A data frame with 24 observations on the following 3 variables:
#' \describe{
#' \item{\code{Depth}}{Depth in cm}
#' \item{\code{RSL}}{Relative sea level in m}
#' \item{\code{Sigma}}{Standard deviation of RSL measurement}
#' }
#' @usage data(TestRSLData)
#' @source Andrew C. Parnell and W. Roland Gehrels (2013) 'Using chronological models in late holocene sea level reconstructions from salt marsh sediments' In: I. Shennan, B.P. Horton, and A.J. Long (eds). Handbook of Sea Level Research. Chichester: Wiley
"TestRSLData"

