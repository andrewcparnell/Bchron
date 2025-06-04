#' Non-parametric phase model (faster version)
#'
#' This function runs a non-parametric phase model on 14C and non-14C ages via Gaussian Mixture density estimation through the mclust package
#'
#' @param ages A vector of ages (most likely 14C)
#' @param ageSds A vector of 1-sigma values for the ages given above
#' @param calCurves A vector of values containing either \code{intcal20}, \code{shcal20}, \code{marine20}, or \code{normal} (older calibration curves such as intcal13 are also supported). Should be the same length the number of ages supplied. Non-standard calibration curves can be used provided they are supplied in the same format as those previously mentioned and are placed in the same directory. Normal indicates a normally-distributed (non-14C) age.
#' @param pathToCalCurves File path to where the calibration curves are located. Defaults to the system directory where the 3 standard calibration curves are stored.
#' @param dfs Degrees-of-freedom values for the t-distribution associated with the calibration calculation. A large value indicates Gaussian distributions assumed for the 14C ages
#' @param samples Number of samples of calibrated dates required
#' @param G Number of Gaussian mixture components
#'
#' @details This is a faster approximate version of \code{\link{BchronDensity}} that uses the \code{\link{densityMclust}} function to compute the Gaussian mixtures for a set of calibrated ages. The method is an approximation as it does not fit a fully Bayesian model as \code{\link{BchronDensity}} does. It is designed to be a probabilistic version of the Oxcal SUM command which takes calibrated ages and sums the probability distributions with the aim of estimating activity through age as a proxy.
#'
#' @return An object of class \code{BchronDensityRunFast} with the following components:
#' \describe{
#' \item{out}{The output from the run of \code{\link{densityMclust}} with the given number of mixture components}
#' \item{calAges}{The calibrated ages from the \code{\link{BchronDensity}} function}
#' }
#' @export
#'
#' @seealso \code{\link{Bchronology}}, \code{\link{BchronCalibrate}}, \code{\link{BchronRSL}}, \code{\link{BchronDensity}} for a slower exact version of this function
#'
#' @examples
#' \donttest{
#' # Read in some data from Sluggan Moss
#' data(Sluggan)
#'
#' # Run the model
#' SlugDensFast <- with(
#'   Sluggan,
#'   BchronDensityFast(
#'     ages = ages,
#'     ageSds = ageSds,
#'     calCurves = calCurves
#'   )
#' )
#'
#' # plot it
#' plot(SlugDensFast)
#' }
BchronDensityFast <-
  function(ages, ageSds, calCurves, pathToCalCurves = system.file("data", package = "Bchron"), dfs = rep(100, length(ages)), samples = 2000, G = 30) {
    if (length(ages) != length(ageSds)) stop("ages and 1-sigma errors must be same length")
    if (length(ages) != length(calCurves)) stop("ages and Calibration curves must be same length")

    # Calibrate ages
    x <- BchronCalibrate(ages = ages, ageSds = ageSds, calCurves = calCurves, pathToCalCurves = pathToCalCurves, dfs = rep(100, length(ages)))

    # Get number of dates
    n <- length(x)

    # Get a huge load of samples from the posteriors here
    thetaBig <- vector(length = n * samples)
    for (i in 1:n) thetaBig[((i - 1) * samples + 1):(i * samples)] <- sample(x[[i]]$ageGrid, size = samples, prob = x[[i]]$densities, replace = TRUE)

    # Now run mclust
    mclustOutput <- mclust::densityMclust(
      data = thetaBig,
      G = G
    )

    output <- list(out = mclustOutput, calAges = x)
    class(output) <- "BchronDensityRunFast"
    return(output)
  }
