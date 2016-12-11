#' Fast radiocarbon calibration
#'
#' A fast function for calibrating large numbers of radiocarbon dates involving multiple calibration curves
#'
#' @param ages A vector of ages (most likely 14C)
#' @param ageSds A vector of 1-sigma values for the ages given above
#' @param calCurves A vector of values containing either 'intcal13', 'shcal13', 'marine13', or 'normal'. Should be the same length the number of ages supplied. Non-standard calibration curves can be used provided they are supplied in the same format as those previously mentioned and are placed in the same directory. Normal indicates a normally-distributed (non-14C) age.
#' @param ids ID names for each age
#' @param positions Position values (e.g. depths) for each age
#' @param pathToCalCurves File path to where the calibration curves are located. Defaults to the system directory where the 3 standard calibration curves are stored.
#' @param eps Cut-off point for density calculation. A value of eps>0 removes ages from the output which have negligible probability density
#' @param dfs Degrees-of-freedom values for the t-distribution associated with the calibration calculation. A large value indicates Gaussian distributions assumed for the 14C ages
#'
#' @details This function provides a direct numerical integration strategy for computing calibrated radiocarbon ages. The steps for each 14C age are approximately as follows:
#' 1) Create a grid of ages covering the range of the calibration curve
#' 2) Calculate the probability of each age according to the 14C age, the standard deviation supplied and the calibration curve
#' 3) Normalise the probabilities so that they sum to 1
#' 4) Remove any probabilities that are less than the value given for eps
#' Multiple calibration curves can be specified so that each 14C age can have a different curve. For ages that are not 14C, use the 'normal' calibration curve which treats the ages as normally distributed with given standard deviation
#'
#'
#' @return A list of lists where each element corresponds to a single age. Each element contains:
#' \itemize{
#' \item{ages}{The original age supplied}
#' \item{ageSDs}{The original age standard deviation supplied}
#' \item{positions}{The position of the age (usually the depth)}
#' \item{calCurves}{The calibration curve used for that age}
#' \item{ageGrid}{A grid of age values over which the density was created}
#' \item{densities}{A vector of probability values indicating the probability value for each element in ageGrid}
#' \item{ageLab}{The label given to the age variable}
#' \item{positionLab}{The label given to the position variable}
#' }
#' 
#' @seealso \code{\link{Bchronology}}, \code{\link{BchronRSL}}, \code{\link{BchronDensity}}, \code{\link{BchronDensityFast}}, \code{\link{CreateCalCurve}}
#' 
#' @export
#'
#' @examples
#' # Calibrate a single age
#' ages1 = BchronCalibrate(ages=11553,ageSds=230,calCurves='intcal13',ids='Date-1')
#' summary(ages1)
#' plot(ages1)
#' 
#' # Calibrate multiple ages with different calibration curves
#' ages2 = BchronCalibrate(ages=c(3445,11553,7456),ageSds=c(50,230,110),
#'                         calCurves=c('intcal13','intcal13','shcal13'))
#' summary(ages2)
#' plot(ages2)
#' 
#' # Calibrate multiple ages with multiple calibration curves and including depth
#' ages3 = BchronCalibrate(ages=c(3445,11553),ageSds=c(50,230),positions=c(100,150),
#'                         calCurves=c('intcal13','normal'))
#' summary(ages3)
#' plot(ages3,withDepths=TRUE)
#' 
BchronCalibrate <-
function(ages,ageSds,calCurves,ids=NULL,positions=NULL,pathToCalCurves=system.file('data',package='Bchron'),eps=1e-5,dfs=rep(100,length(ages))) {

  # This function expects ages in years BP (either 14C or not depending on calCurve values)
  # and positions (usually depths) in cm
  # It scales these by ageScaleVal and positionScaleVal respectively to calibrate and then
  # returns them in the original units

  # Check lengths of everything
  if(length(ages)!=length(ageSds)) stop("ages and ageSds should be of same length")
  if(length(ages)!=length(calCurves)) stop("ages and calCurves should be of same length")
  if(!is.null(positions)) if(length(ages)!=length(positions)) stop("ages and positions should be of same length")
  if(is.null(ids)) ids = paste('Date',1:length(ages),sep='')

  # Check that ages and ageSds are whole numbers (i.e. years)
  if(!all(as.integer(ages)==ages)) {
    ages = round(ages,0)
    warning("ages not given as whole numbers - rounding occurred")
  }
  if(!all(as.integer(ageSds)==ageSds)) {
    # Smallest sd is 1
    ageSds = pmax(round(ageSds,0),1)
    warning("ageSds not given as whole numbers - rounding occurred")
  }

  # Load in all calibration curves specified
  allCalCurves = unique(calCurves)
  calCurve = calBP = c14BP = calSd = ageGrid = mu = tau1 = list()
  for(i in 1:length(allCalCurves)) {
    calCurveFile = paste(pathToCalCurves,'/',allCalCurves[i],'.rda',sep='')
    if(!file.exists(calCurveFile)) stop(paste('Calibration curve file',calCurveFile,'not found'))
    calCurve = as.matrix(load(calCurveFile))
    calBP[[i]] = calCurve[,1]
    c14BP[[i]] = calCurve[,2]
    calSd[[i]] = calCurve[,3]
    # Create an age grid and get mean and variance of calibration curve
    ageGrid[[i]] = round(seq(min(calBP[[i]]),max(calBP[[i]]),by=1),0)
    mu[[i]] = stats::approx(calBP[[i]],c14BP[[i]],xout=ageGrid[[i]],rule=2)$y
    tau1[[i]] = stats::approx(calBP[[i]],calSd[[i]],xout=ageGrid[[i]],rule=2)$y
    # Allow for greater age ranges if the calibration curve is normal
    if(allCalCurves[i]=='normal') {
      ageRange = range(c(calBP,ages+4*ageSds))
      ageGrid[[i]] = seq(ageRange[1],ageRange[2],by=1)
      mu[[i]] = ageGrid[[i]]
      tau1[[i]] = rep(0,length(ageGrid[[i]]))
    }

  }
  matchCalCurves = match(calCurves,allCalCurves)

  # Storage
  out = list()

  # Loop through ages and calculate densities
  for(i in 1:length(ages)) {

    # Get rid of ages outside the range of the uncalibrated dates
    if(ages[i]>max(mu[[matchCalCurves[i]]]) | ages[i]<min(mu[[matchCalCurves[i]]])) {
      cal_range = range(mu[[matchCalCurves[i]]])
      stop(paste("Date",ids[i],"outside of calibration range. Range of", calCurves[i], 'is',cal_range[1],'to',cal_range[2]))
    }

    tau = ageSds[i]^2 + tau1[[matchCalCurves[i]]]

    currAgeGrid = ageGrid[[matchCalCurves[i]]]
    dens = stats::dt((ages[i]-mu[[matchCalCurves[i]]])/sqrt(tau),df=dfs[i])
    dens = dens/sum(dens)

    # Create list of output
    if(is.null(positions)) {
      out[[i]] = list(ages=ages[i],ageSds=ageSds[i],calCurves=calCurves[i],ageGrid=currAgeGrid[dens>eps],densities=dens[dens>eps])
    } else {
      out[[i]] = list(ages=ages[i],ageSds=ageSds[i],positions=positions[i],calCurves=calCurves[i],ageGrid=currAgeGrid[dens>eps],densities=dens[dens>eps])

    }
  }

  names(out) = ids
  class(out) = 'BchronCalibratedDates'
  return(out)

}
