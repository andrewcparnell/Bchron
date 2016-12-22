#' Uncalibrate a Radiocarbon date
#'
#' @param calAges Either a vector of calibrated ages (when type = 'ages'), or a vector of calibrated samples (type = 'samples')
#' @param calCurve he calibration curve to use. Only a single calibration curve is currently supported
#' @param type Either 'ages' which uncalibrates a calibrated age values without error (i.e. just a lookup on the calibration curve), or a 'samples' which estimates both an uncalibrated mean age and a standard deviation
#' @param pathToCalCurves The path to the calibration curve directory. Defaults to the location of the standard calibration curves given in the package
#' @param ... Other arguements to the \link{\code{optim}} function used to match the probability dsitributions under \code{type = 'samples'}
#'
#' @return Eitehr a vector of uncalibrated ages (\code{type = 'ages'}) or a list containing the estimated mean age and standard deviation (\code{type = 'samples'})
#' @export
#'
#' @examples
#' # Single version outputting just an uncalibrated age
#' unCalibrate(2350, type = 'ages')
#' 
#' # Vector version giving a vector of uncalibrated ages
#' unCalibrate(calAge = c(2350, 4750, 11440),
#'   calCurve = 'shcal13',
#'   type = 'ages')
#' 
#' # A version where calibrated standard deviations are required too
#' calAge = BchronCalibrate(ages = 11255,
#'   ageSds = 25,
#'   calCurves = 'intcal13')
#' calSampleAges = sampleAges(calAge)
#' 
#' # Uncalibrate the above
#' unCalibrate(calSampleAges,
#'   type = 'samples')
unCalibrate = function(calAges,
                       calCurve = 'intcal13',
                       type = c('samples', 'ages'),
                       pathToCalCurves = system.file('data', package = 'Bchron'),
                       ...) {
  
  # First get the calibration curve
  calCurveFile = paste(pathToCalCurves,'/',calCurve,'.rda',sep='')
  if(!file.exists(calCurveFile)) stop(paste('Calibration curve file',calCurveFile,'not found'))
  x = load(calCurveFile)
  currCalCurve = get(x)
  
  # Sort out into useful columns
  calBP = currCalCurve[,1]
  c14BP = currCalCurve[,2]

  # Simple case first
  if(type == 'ages') {
    # Get ready to store ages
    unCalAges = rep(NA, length(calAges))
    # Start up the progress bar
    pb = utils::txtProgressBar(min = 1,
                               max = length(calAges)+1,
                               style = 3,
                               width = 0.8*options()$width,
                               title = 'Uncalibrating...')
    # Now loop through each age and uncalibrate
    for(i in 1:length(calAges)) {
      # Progress bar
      utils::setTxtProgressBar(pb, i+1)
      
      # Approx uncal age - rule = 1 specifies NAs for extrapolation
      unCalAges[i] = approx(calBP, 
                            c14BP, 
                            xout = calAges[i],
                            rule = 1)$y
    }
    return(unCalAges)
  }
  
  if(type == 'samples') {
    stopifnot(length(calAges) != 1)
    # Optimise KL divergence between samples and calibrated age
    # KL divergence defined as sum_i p(i) log(p(i)/q(i)
    
    # Get extra arguments
    ex = list(...)
    if(is.null(ex$method)) ex$method = 'Nelder-Mead' #'SANN'
    
    # Write function for an optim type command
    opt_fun = function(pars, samples) {
      s1 = samples
      calDate = BchronCalibrate(ages = round(pars[1]),
                                ageSds = round(pars[2]),
                                calCurves = calCurve)
      s2 = sampleAges(calDate)
      
      # Calculate densities
      s1Dens = density(s1, 
                       from = min(c(s1, s2)), 
                       to = max(c(s1, s2)))$y
      s2Dens = density(s2, 
                       from = min(c(s1, s2)), 
                       to = max(c(s1, s2)))$y
      
      # Re-scale them so that they sum to 1
      s1DensResc = s1Dens/sum(s1Dens)
      s2DensResc = s2Dens/sum(s2Dens)
      
      # Calculate the KL divergence
      int = s1DensResc*log(s1DensResc/s2DensResc)
      # Get rid of NA values created above
      int = int[s1DensResc>0]
      int = int[!is.infinite(int)]
      return(sum(int))
    }
    
    # Get initial guesses
    init_mean = unCalibrate(median(calAges),
                            calCurve = calCurve,
                            type = 'ages')
    cat('\n')
    init_sd = sd(calAges)
    
    # Test function
    #opt_fun(round(c(init_mean, init_sd)), calAges)
    
    # Run optimisation
    opt_run = stats::optim(par = round(c(init_mean, init_sd)),
                    fn = opt_fun,
                    method = ex$method,
                    samples = calAges)#,
                    #lower = c(0,0), 
                    #upper = c(max(c14BP), 500))
    
    out = opt_run$par
    return(list(mean = round(out[1]), sd = round(out[2])))
  }
  
}

