#' Uncalibrate a Radiocarbon date
#' 
#' Turns a calibrated age estimate into an uncalibrated age based on a given calibration curve
#'
#' @param calAge The calibrated age value (can be a vector)
#' @param calCurve The calibration curve to use. Must be the same length as \code{calAge}
#' @param pathToCalCurves The path to the calibration curve directory. Defaults to the location of the standard calibration curves given in the package.
#'
#' @return A vector of uncalibrated ages
#' @export
#'
#' @examples
#' unCalibrate(2350)
#' 
#' unCalibrate(calAge = c(2350, 4750, 11440), 
#' calCurve = c('marine13', 'intcal13', 'shcal13'))
unCalibrate = function(calAge, 
                       calCurve = rep('intcal13',
                                      length(calAge)),
                       pathToCalCurves = system.file('data', package = 'Bchron')) {

  # Length of first two arguments must be the same
  stopifnot(length(calAge) == length(calCurve))
  
  # Get ready to store ages
  unCalAges = rep(NA, length(calAge))

  # Start up the progress bar
  pb = utils::txtProgressBar(min = 1,
                             max = length(calCurve)+1,
                             style = 3,
                             width = 60,
                             title = 'Uncalibrating...')
  
  # Get the calibration curve for each one and unCalibrate
  allCalCurves = unique(calCurve)
  matchCal = match(calCurve, allCalCurves)
  calBP = c14BP = vector('list', length = length(allCalCurves))
  for(i in 1:length(allCalCurves)) {
    calCurveFile = paste(pathToCalCurves,'/',calCurve[i],'.rda',sep='')
    if(!file.exists(calCurveFile)) stop(paste('Calibration curve file',calCurveFile,'not found'))
    x = load(calCurveFile)
    currCalCurve = get(x)
    
    # Sort out into useful columns
    calBP[[i]] = currCalCurve[,1]
    c14BP[[i]] = currCalCurve[,2]
  }
  
  for(i in 1:length(calCurve)) {
    # Progress bar
    utils::setTxtProgressBar(pb, i+1)
    
    # Approx uncal age - rule = 1 specifies NAs for extrapolation
    unCalAges[i] = approx(calBP[[matchCal[i]]], 
                          c14BP[[matchCal[i]]], 
                          xout = calAge[i],
                          rule = 1)$y
  }
  cat('\n')
  return(unCalAges)
}

