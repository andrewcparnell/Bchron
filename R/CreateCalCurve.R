#' Create a new calibration curve
#'
#' A function for creating a new calibration curve not already avialable in Bchron
#'
#' @param name The name of the new calibration curve
#' @param cal_ages A vector of the calendar/calibrated ages in years before present
#' @param uncal_ages A vector of values of uncalibrated ages in appropriate units (e.g. 14C years BP)
#' @param one_sigma The one sigma (one standard devation) values in uncalibrated units. If left blank it assumes these are all zero
#'
#' @details Calibration curves are stored by Bchron in the standard R gzipped text format. You can find the location of the calibration curves by typing \code{system.file('data',package='Bchron')}. Any calibration curve supplied will be converted to this format and put in the appropriate directory. It can then be used as in the examples below. However note that re-installing Bchron will likely over-write previously created calibration curves so you should make sure to store the code used to create it.
#'
#' @seealso 
#' \code{\link{BchronCalibrate}}
#' @export
#'
#' @examples
#' \dontrun{
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
CreateCalCurve = function(name,cal_ages,uncal_ages,one_sigma=rep(0,length(cal_ages))) {

  # This function creates a calibration curve and puts it in the appropriate place for future use with Bchron

  # First identify the place where the file needs to go
  file_loc = system.file('data',package='Bchron')

  # Now interpolate so that everything is on a regular grid in calendar years
  cal_order = order(cal_ages,decreasing=TRUE)
  out = cbind(cal_ages[cal_order],uncal_ages[cal_order],one_sigma[cal_order])

  # Now write to a gzipped file
  gz1 = gzfile(paste0(file_loc,'/',name,'.txt.gz'),"w")
  for(i in 1:nrow(out)) write(out[i,],gz1)
  close(gz1)

  # And we're done
  cat('Completed!\n')
}