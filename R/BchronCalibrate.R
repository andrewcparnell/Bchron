#' Fast radiocarbon calibration
#'
#' A fast function for calibrating large numbers of radiocarbon dates involving multiple calibration curves
#'
#' @param ages A vector of ages provided in years before 1950.
#' @param ageSds A vector of 1-sigma values for the ages given above
#' @param calCurves A vector of values containing either \code{intcal20}, \code{shcal20}, \code{marine20}, or \code{normal} (older calibration curves are supposed such as intcal13). Should be the same length the number of ages supplied. Non-standard calibration curves can be used provided they are supplied in the same format as those previously mentioned and are placed in the same directory. Normal indicates a normally-distributed (non-14C) age.
#' @param ids ID names for each age
#' @param positions Position values (e.g. depths) for each age. In the case of layers of non-zero thickness, this should be the middle value of the slice
#' @param pathToCalCurves File path to where the calibration curves are located. Defaults to the system directory where the 3 standard calibration curves are stored.
#' @param allowOutside Whether to allow calibrations to run outside the range of the calibration curve. By default this is turned off as calibrations outside of the range of the calibration curve can cause severe issues with probability ranges of calibrated dates
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
#' \describe{
#'  \item{ages}{The original age supplied}
#'  \item{ageSds}{The original age standard deviation supplied}
#'  \item{positions}{The position of the age (usually the depth)}
#'  \item{calCurves}{The calibration curve used for that age}
#'  \item{ageGrid}{A grid of age values over which the density was created}
#'  \item{densities}{A vector of probability values indicating the probability value for each element in \code{ageGrid}}
#'  \item{ageLab}{The label given to the age variable}
#'  \item{positionLab}{The label given to the position variable}
#' }
#'
#' @seealso \code{\link{Bchronology}}, \code{\link{BchronRSL}}, \code{\link{BchronDensity}}, \code{\link{BchronDensityFast}}, \code{\link{createCalCurve}}
#'
#' @export
#'
#' @examples
#' # Calibrate a single age
#' ages1 <- BchronCalibrate(
#'   ages = 11553,
#'   ageSds = 230,
#'   calCurves = "intcal20",
#'   ids = "Date-1"
#' )
#' summary(ages1)
#' plot(ages1)
#'
#' # Or plot with Calibration curve
#' plot(ages1, includeCal = TRUE)
#'
#' # Calibrate multiple ages with different calibration curves
#' ages2 <- BchronCalibrate(
#'   ages = c(3445, 11553, 7456),
#'   ageSds = c(50, 230, 110),
#'   calCurves = c("intcal20", "intcal20", "shcal20")
#' )
#' summary(ages2)
#' plot(ages2)
#'
#' # Calibrate multiple ages with multiple calibration curves and including depth
#' ages3 <- BchronCalibrate(
#'   ages = c(3445, 11553),
#'   ageSds = c(50, 230),
#'   positions = c(100, 150),
#'   calCurves = c("intcal20", "normal")
#' )
#' summary(ages3)
#' plot(ages3, withPositions = TRUE)
BchronCalibrate <- function(ages,
                            ageSds,
                            calCurves = rep("intcal20", length(ages)),
                            ids = NULL,
                            positions = NULL,
                            pathToCalCurves = system.file("data",
                              package = "Bchron"
                            ),
                            allowOutside = FALSE,
                            eps = 1e-5,
                            dfs = rep(100, length(ages))) {

  # This function expects ages in years BP (either 14C or not depending on calCurve values)
  # and positions (usually depths) in cm

  # Run the Bchron check function in case things have gone wrong
  BchronCheck(
    ages = ages,
    ageSds = ageSds,
    calCurves = calCurves,
    ids = ids,
    positions = positions,
    pathToCalCurves = pathToCalCurves,
    eps = eps,
    dfs = dfs,
    type = "BchronCalibrate"
  )

  # If the calCurves argument was not provided print it out
  if (missing(calCurves)) message(paste("Calibrating curve not provided. Using:", unique(calCurves), "\n"))

  # Insert ids if NULL
  if (is.null(ids)) ids <- paste("Date", 1:length(ages), sep = "")
  # Round ages to ensure whole numbers - these will be notified in BchronCheck
  ages <- round(ages, 0)
  ageSds <- pmax(round(ageSds, 0), 1)

  # Load in all calibration curves specified
  allCalCurves <- unique(calCurves)
  calCurve <- calBP <- c14BP <- calSd <- ageGrid <- mu <- tau1 <- list()
  for (i in 1:length(allCalCurves)) {
    calCurveFile <- paste(pathToCalCurves, "/", allCalCurves[i], ".rda", sep = "")
    x <- load(calCurveFile)
    calCurve <- get(x)
    calBP[[i]] <- calCurve[, 1]
    c14BP[[i]] <- calCurve[, 2]
    calSd[[i]] <- calCurve[, 3]
    # Create an age grid and get mean and variance of calibration curve
    ageGrid[[i]] <- round(seq(min(calBP[[i]]), max(calBP[[i]]), by = 1), 0)
    mu[[i]] <- stats::approx(calBP[[i]], c14BP[[i]], xout = ageGrid[[i]], rule = 2)$y
    tau1[[i]] <- stats::approx(calBP[[i]], calSd[[i]], xout = ageGrid[[i]], rule = 2)$y
    # Allow for greater age ranges if the calibration curve is normal
    if (allCalCurves[i] == "normal") {
      ageRange <- range(c(calBP, ages + 4 * ageSds))
      ageGrid[[i]] <- seq(ageRange[1], ageRange[2], by = 1)
      mu[[i]] <- ageGrid[[i]]
      tau1[[i]] <- rep(0, length(ageGrid[[i]]))
    }
  }
  matchCalCurves <- match(calCurves, allCalCurves)

  # Storage
  out <- list()

  # Loop through ages and calculate densities
  for (i in 1:length(ages)) {

    # Get rid of ages outside the range of the uncalibrated dates
    if (!allowOutside) {
      if (ages[i] > max(mu[[matchCalCurves[i]]]) | ages[i] < min(mu[[matchCalCurves[i]]])) {
        cal_range <- range(mu[[matchCalCurves[i]]])
        stop(paste0("Date ID ", ids[i], " outside of calibration range. Range of ", calCurves[i], " is ", cal_range[1], " to ", cal_range[2], ". Set allowOutside = TRUE if you want to calibrate ages outside of the calibration curve range"))
      }
    }

    tau <- ageSds[i]^2 + tau1[[matchCalCurves[i]]]^2

    currAgeGrid <- ageGrid[[matchCalCurves[i]]]
    dens <- stats::dt((ages[i] - mu[[matchCalCurves[i]]]) / sqrt(tau), df = dfs[i])
    # dens = stats::dnorm((ages[i]-mu[[matchCalCurves[i]]])/sqrt(tau)) # Version should match OxCal and others (though I would argue that t-distribution is better)
    dens <- dens / sum(dens)

    # Create list of output
    if(length(currAgeGrid[dens > eps]) == 0) warning(paste("Age grid and densities for date", ids[i],"are of length 0. Try reducing the value of argument eps in BchronCalibrate by a factor of 10 \n"))
    if (is.null(positions)) {
      out[[i]] <- list(ages = ages[i], ageSds = ageSds[i], calCurves = calCurves[i], ageGrid = currAgeGrid[dens > eps], densities = dens[dens > eps])
    } else {
      out[[i]] <- list(ages = ages[i], ageSds = ageSds[i], positions = positions[i], calCurves = calCurves[i], ageGrid = currAgeGrid[dens > eps], densities = dens[dens > eps])
      
    }
  }

  names(out) <- ids
  class(out) <- "BchronCalibratedDates"
  return(out)
}
