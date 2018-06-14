#' Create a new calibration curve
#'
#' A function for creating a new calibration curve not already avialable in Bchron
#'
#' @param name The name of the new calibration curve
#' @param cal_ages A vector of the calendar/calibrated ages in years before present
#' @param uncal_ages A vector of values of uncalibrated ages in appropriate units (e.g. 14C years BP)
#' @param one_sigma The one sigma (one standard devation) values in uncalibrated units. If left blank it assumes these are all zero
#' @param path_to_cal_curves The path to the calibration curves. Will write by default to the working directory
#'
#' @details All calibration curves are stored by Bchron in the standard R gzipped text format. You can find the location of the calibration curves by typing \code{system.file('data',package='Bchron')}. Any created calibration curve will be converted to this format. However R packages are not allowed to write to this directory so it is up to the user to put the resulting calibration curve file in the appropriate directory. It can then be used as in the examples below. However note that re-installing Bchron will likely over-write previously created calibration curves so you should make sure to store the code used to create it.
#'
#' @seealso 
#' \code{\link{BchronCalibrate}}
#' @export
#'
#' @examples
#' \donttest{
#' # Load in the calibration curve with:
#' intcal09 = read.table('http://www.radiocarbon.org/IntCal09%20files/intcal09.14c',sep=',')
#' # Run CreateCalCurve
#' CreateCalCurve(name='intcal09',cal_ages=intcal09[,1],uncal_ages=intcal09[,2],one_sigma=intcal09[,3])
#' 
#' # Calibrate the ages under two calibration curves
#' age_09 = BchronCalibrate(age=15500,ageSds=150,calCurves = 'intcal09',ids='My Date')
#' age_13 = BchronCalibrate(age=15500,ageSds=150,calCurves = 'intcal13')
#' 
#' # Finally plot the difference
#' plot(age_09)
#' with(age_13$date1,lines(ageGrid,densities,col='red'))
#' legend('topleft',legend=c('intcal09','intcal13'),col=c('black','red'),lty=1)
#' }
#' 
CreateCalCurve = function(name,
                          cal_ages,
                          uncal_ages,
                          one_sigma=rep(0,length(cal_ages)),
                          path_to_cal_curves = getwd()) {

  # This function creates a calibration curve and puts it in the appropriate place for future use with Bchron

  # First identify the place where the file needs to go
  file_loc = path_to_cal_curves

  # Now interpolate so that everything is on a regular grid in calendar years
  cal_order = order(cal_ages,decreasing=TRUE)
  out = cbind(cal_ages[cal_order],uncal_ages[cal_order],one_sigma[cal_order])

  # Now write to an rda file
  fl = paste0(file_loc,'/',name,'.rda')
  save(out, file = fl)
  
  # And we're done
  cat('Completed!\n')
}