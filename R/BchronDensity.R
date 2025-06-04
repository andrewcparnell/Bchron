#' Non-parametric phase model
#'
#' This function runs a non-parametric phase model on 14C and non-14C ages via Gaussian Mixture density estimation
#'
#' @param ages A vector of ages (most likely 14C)
#' @param ageSds A vector of 1-sigma values for the ages given above
#' @param calCurves A vector of values containing either \code{intcal20}, \code{shcal20}, \code{marine20}, or \code{normal} (older calibration curves such as intcal13 are also supported). Should be the same length the number of ages supplied. Non-standard calibration curves can be used provided they are supplied in the same format as those previously mentioned and are placed in the same directory. Normal indicates a normally-distributed (non-14C) age.
#' @param pathToCalCurves File path to where the calibration curves are located. Defaults to the system directory where the 3 standard calibration curves are stored
#' @param dfs Degrees-of-freedom values for the t-distribution associated with the calibration calculation. A large value indicates Gaussian distributions assumed for the 14C ages
#' @param numMix The number of mixture components in the phase model. Might need to be increased if the data set is large and the phase behaviour is very complex
#' @param iterations The number of iterations to run for
#' @param burn The number of starting iterations to discard
#' @param thin The step size of iterations to keep
#' @param updateAges Whether or not to update ages as part of the MCMC run. Default is FALSE. Changing this to TRUE will improve performance but will fit a slightly invalid model
#' @param store_density Whether or not to store the density and age grid. Useful for plotting the output in other packages
#'
#' @details This model places a Gaussian mixture prior distribution on the calibrated ages and so estimates the density of the overall set of radiocarbon ages. It is designed to be a probabilistic version of the Oxcal SUM command which takes calibrated ages and sums the probability distributions with the aim of estimating activity through age as a proxy.
#'
#' @return An object of class \code{BchronDensityRun} with the following elements:
#' \describe{
#' \item{theta}{The posterior samples of the restricted ages}
#' \item{p}{Posterior samples of the mixture proportions}
#' \item{mu}{Values of the means of each Gaussian mixture}
#' \item{calAges}{The calibrated ages from \code{\link{BchronCalibrate}}}
#' \item{G}{The number of mixture components. Equal to numMix}
#' \item{age_grid}{A grid of ages used for the final density estimate}
#' \item{density}{The density estimate based on the above age grid}
#' }
#'
#' @seealso \code{\link{Bchronology}}, \code{\link{BchronRSL}}, \code{\link{BchronDensityFast}} for a faster approximate version of this function
#'
#' @export
#'
#' @examples
#' \donttest{
#' # Read in some data from Sluggan Moss
#' data(Sluggan)
#'
#' # Run the model
#' SlugDens <- with(
#'   Sluggan,
#'   BchronDensity(
#'     ages = ages,
#'     ageSds = ageSds,
#'     calCurves = calCurves
#'   )
#' )
#'
#' # plot it
#' plot(SlugDens)
#' }
BchronDensity <-
  function(ages, ageSds, calCurves, pathToCalCurves = system.file("data", package = "Bchron"), dfs = rep(100, length(ages)), numMix = 50, iterations = 10000, burn = 2000, thin = 8, updateAges = FALSE, store_density = TRUE) {
    if (length(ages) != length(ageSds)) stop("ages and 1-sigma errors must be same length")
    if (length(ages) != length(calCurves)) stop("ages and Calibration curves must be same length")

    # Calibrate ages
    x <- BchronCalibrate(ages = ages, ageSds = ageSds, calCurves = calCurves, pathToCalCurves = pathToCalCurves, eps = 0, dfs = rep(100, length(ages)))
    xSmall <- BchronCalibrate(ages = ages, ageSds = ageSds, calCurves = calCurves, pathToCalCurves = pathToCalCurves, dfs = rep(100, length(ages)))

    # Get thetaRange to calculate mu values
    n <- length(x)
    thetaRange <- range(xSmall[[1]]$ageGrid)
    for (i in 2:n) thetaRange <- range(c(thetaRange, xSmall[[i]]$ageGrid))

    # Put in offset for normal calibration curve (enables faster lookup)
    offset <- vector(length = n)
    for (i in 1:n) {
      offset[i] <- ifelse(x[[i]]$calCurve == "normal", 100, 0)
    }

    # Create some Gaussian basis functions to use
    gauss <- function(x, mu, sig) {
      # Gaussian-shaped function
      u <- (x - mu) / sig
      y <- exp(-u * u / 2)
      y
    }

    gbase <- function(x, mus) {
      # Construct Gaussian basis
      sig <- (mus[2] - mus[1]) / 2
      G <- outer(x, mus, gauss, sig)
      G
    }

    clrInv <- function(phi) {
      return(exp(phi) / sum(exp(phi)))
    }

    # Starting values
    J <- numMix
    mu <- seq(thetaRange[1], thetaRange[2], length = numMix)
    theta <- vector(length = n)
    for (j in 1:n) theta[j] <- round(stats::rnorm(1, mean = x[[j]]$ageGrid[match(max(x[[j]]$densities), x[[j]]$densities)], sd = ageSds[j]), 3)
    phi <- c(stats::runif(J - 1, -10, 10), 0)
    p <- as.numeric(clrInv(phi))
    G <- gbase(theta, mu)

    # Storage
    remaining <- (iterations - burn) / thin
    thetaStore <- matrix(ncol = length(theta), nrow = remaining)
    pStore <- matrix(ncol = J, nrow = remaining)

    # Get thetas up front for faster results
    thetaAll <- matrix(NA, ncol = n, nrow = iterations)
    for (j in 1:n) thetaAll[, j] <- sample(xSmall[[j]]$ageGrid, size = iterations, prob = xSmall[[j]]$densities, replace = TRUE)

    # Create function for quick calling of mixture density
    mu2 <- mu
    sigma2 <- (mu[2] - mu[1]) / 2

    # Loop through iterations
    pb <- utils::txtProgressBar(min = 1, max = iterations, style = 3, width = 60, title = "Running BchronDensity")
    for (i in 1:iterations) {
      utils::setTxtProgressBar(pb, i)

      # Store stuff
      if (i > burn & i %% thin == 0) {
        ind <- (i - burn) / thin
        thetaStore[ind, ] <- theta
        pStore[ind, ] <- p
      }

      # Update theta
      if (updateAges) {
        for (j in 1:n) {
          thetaNew <- round(stats::rnorm(1, theta[j], 0.5), 3)
          thetaNewMatch <- as.integer(thetaNew + offset[j]) + 1
          thetaNewLogDens <- max(log(x[[j]]$densities[thetaNewMatch]), -1000000)
          priorNew.dens <- sum(p * stats::dnorm(thetaNew, mean = mu2, sd = sigma2))
          thetaMatch <- as.integer(theta[j] + offset[j]) + 1
          thetaLogDens <- max(log(x[[j]]$densities[thetaMatch]), -1000000)
          priorDens <- sum(p * stats::dnorm(theta[j], mean = mu2, sd = sigma2))

          logRtheta <- thetaNewLogDens - thetaLogDens + log(priorNew.dens) - log(priorDens)
          if (stats::runif(1) < exp(logRtheta)) theta[j] <- thetaNew
        }
      } else {
        theta <- thetaAll[i, ]
      }

      # Update phi
      for (j in 1:(J - 1)) {
        phiNew <- stats::rnorm(1, phi[j], 1)
        phiAllNew <- phi
        phiAllNew[j] <- phiNew
        pNew <- as.numeric(clrInv(phiAllNew))
        phiNewLogDens <- sum(log(G %*% pNew))
        phiLogDens <- sum(log(G %*% p))
        logRphi <- phiNewLogDens - phiLogDens + stats::dunif(phiNew, -10, 10, log = TRUE) - stats::dunif(phi[j], -10, 10, log = TRUE)
        if (stats::runif(1) < exp(logRphi)) {
          phi[j] <- phiNew
          p <- as.numeric(clrInv(phi))
        }
      }
    }

    # Store the density if required
    if (store_density) {
      # Create the densities
      dateGrid <- seq(round(thetaRange[1] * 0.9, 0), round(thetaRange[2] * 1.1, 0), by = 1)
      dens <- rep(0, length = length(dateGrid))
      Gstar <- gbase(dateGrid, mu)
      for (i in 1:nrow(pStore)) {
        dens <- dens + Gstar %*% pStore[i, ]
      }
      densFinal <- dens / sum(dens)
    }

    if (store_density) {
      output <- list(theta = thetaStore, p = pStore, mu = mu, calAges = xSmall, G = G, ageGrid = dateGrid, densities = densFinal)
    } else {
      output <- list(theta = thetaStore, p = pStore, mu = mu, calAges = xSmall, G = G)
    }

    class(output) <- "BchronDensityRun"
    return(output)
  }
