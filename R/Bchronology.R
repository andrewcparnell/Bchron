#' Runs the Compound Poisson-Gamma chronology model of Haslett and Parnell (2008)
#'
#' Fits a non-parametric chronology model to age/position data according to the Compound Poisson-Gamma model defined by Haslett and Parnell (2008) <DOI:10.1111/j.1467-9876.2008.00623.x>. This version uses a slightly modified Markov chain Monte Carlo fitting algorithm which aims to converge quicker and requires fewer iterations. It also a slightly modified procedure for identifying outliers
#'
#' @inheritParams BchronCalibrate
#'
#' @param positionThicknesses Thickness values for each of the positions. The thickness value should be the full thickness value of the slice. By default set to zero.
#' @param outlierProbs A vector of prior outlier probabilities, one for each age. Defaults to 0.01
#' @param predictPositions A vector of positions (e.g. depths) at which predicted age values are required. Defaults to a sequence of length 100 from the top position to the bottom position
#' @param artificialThickness Amount to add to the thickness values in the case of equal positions with no \code{positionThicknesses}. Bchron may fail if \code{positionThicknesses} are zero and some positions are repeated. This value is added on to the zero thicknesses (only in the case of repeated positions) to stop this failure.
#' @param iterations The number of iterations to run the procedure for
#' @param burn The number of starting iterations to discard
#' @param thin The step size for every iteration to keep beyond the burn-in
#' @param extractDate The top age of the core. Used for extrapolation purposes so that no extrapolated ages go beyond the top age of the core. Defaults to the current year
#' @param maxExtrap The maximum number of extrapolations to perform before giving up and setting the predicted ages to NA. Useful for when large amounts of extrapolation are required, i.e. some of the \code{predictPositions} are a long way from the dated positions
#' @param thetaStart A set of starting values for the calendar ages estimated by Bchron. If NULL uses a function to estimate the ages. These should be in the same units as the posterior ages required. See example below for usage.
#' @param thetaMhSd The Metropolis-Hastings standard deviation for the age parameters
#' @param muMhSd The Metropolis-Hastings standard deviation for the Compound Poisson-Gamma mean
#' @param psiMhSd The Metropolis-Hastings standard deviation for the Compound Poisson-Gamma scale
#' @param ageScaleVal A scale value for the ages. \code{Bchronology} works best when the ages are scaled to be approximately between 0 and 100. The default value is thus 1000 for ages given in years.
#' @param positionEps A small value used to check whether simulated positions are far enough apart to avoid numerical underflow errors. If errors occur in model runs (e.g. \code{missing value where TRUE/FALSE needed} increase this value)
#' @param positionNormalise Whether to normalise the position values. \code{Bchronology} works best when the positions are normalised to be between 0 and 1 The default value is \code{TRUE}
#'
#' @details
#' The \code{Bchronology} function fits a compound Poisson-Gamma distribution to the increments between the dated levels. This involves a stochastic linear interpolation step where the age gaps are Gamma distributed, and the position gaps are Exponential. Radiocarbon and non-radiocarbon dates (including outliers) are updated within the function also by MCMC.
#'
#' @useDynLib Bchron
#'
#' @return A list of class \code{BchronologyRun} which include elements:
#'  \item{theta}{The posterior estimated values of the ages}
#'  \item{phi}{The posterior estimated outlier values (1=outlier, 2=not outlier). The means of this parameter give the posterior estimated outlier probabilities}
#'  \item{mu}{The posterior values of the Compound Poisson-Gamma mean}
#'  \item{psi}{The posterior values of the Compound Poisson-Gamma scale}
#'  \item{thetaPredict}{The posterior estimated ages for each of the values in predictPosition}
#'  \item{predictPositions}{The positions at which estimated ages were required}
#'  \item{calAges}{The calibrated ages as output from \code{\link{BchronCalibrate}}}
#'  \item{inputVals}{All of the input values to the \code{Bchronology} run}
#'
#' @references
#' Haslett, J., and Parnell, A. C. (2008). A simple monotone process with application to radiocarbon-dated depth chronologies. Journal of the Royal Statistical Society, Series C, 57, 399-418. DOI:10.1111/j.1467-9876.2008.00623.x
#' Parnell, A. C., Haslett, J., Allen, J. R. M., Buck, C. E., and Huntley, B. (2008). A flexible approach to assessing synchroneity of past events using Bayesian reconstructions of sedimentation history. Quaternary Science Reviews, 27(19-20), 1872-1885. DOI:10.1016/j.quascirev.2008.07.009
#'
#' @seealso
#' \code{\link{BchronCalibrate}}, \code{\link{BchronRSL}}, \code{\link{BchronDensity}}, \code{\link{BchronDensityFast}}
#' @examples
#' \donttest{
#' # Data from Glendalough
#' data(Glendalough)
#'
#' # Run in Bchronology - all but first age uses intcal20
#' GlenOut <- with(
#'   Glendalough,
#'   Bchronology(
#'     ages = ages,
#'     ageSds = ageSds,
#'     calCurves = calCurves,
#'     positions = position,
#'     positionThicknesses = thickness,
#'     ids = id,
#'     predictPositions = seq(0, 1500, by = 10)
#'   )
#' )
#'
#' # Summarise it a few different ways
#' summary(GlenOut) # Default is for quantiles of ages at predictPosition values
#' summary(GlenOut, type = "convergence") # Check model convergence
#' summary(GlenOut, type = "outliers") # Look at outlier probabilities
#'
#' # Predict for some new positions
#' predictAges <- predict(GlenOut,
#'   newPositions = c(150, 725, 1500),
#'   newPositionThicknesses = c(5, 0, 20)
#' )
#'
#' # Plot the output
#' plot(GlenOut) +
#'   ggplot2::labs(
#'     title = "Glendalough",
#'     xlab = "Age (cal years BP)",
#'     ylab = "Depth (cm)"
#'   )
#'
#' # If you need to specify your own starting values
#' startingAges <- c(0, 2000, 10000, 11000, 13000, 13500)
#' GlenOut <- with(
#'   Glendalough,
#'   Bchronology(
#'     ages = ages,
#'     ageSds = ageSds,
#'     calCurves = calCurves,
#'     positions = position,
#'     positionThicknesses = thickness,
#'     ids = id,
#'     predictPositions = seq(0, 1500, by = 10),
#'     thetaStart = startingAges
#'   )
#' )
#' }
#'
#' @export
Bchronology <- function(ages,
                        ageSds,
                        positions,
                        positionThicknesses = rep(
                          0,
                          length(ages)
                        ),
                        calCurves = rep(
                          "intcal20",
                          length(ages)
                        ),
                        ids = NULL,
                        outlierProbs = rep(0.01, length(ages)),
                        predictPositions = seq(min(positions),
                          max(positions),
                          length  = 100
                        ),
                        pathToCalCurves = system.file("data", package = "Bchron"),
                        artificialThickness = 0.01,
                        allowOutside = FALSE,
                        iterations = 10000,
                        burn = 2000,
                        thin = 8,
                        extractDate = 1950 - as.numeric(format(Sys.time(), "%Y")),
                        maxExtrap = 1000,
                        thetaStart = NULL,
                        thetaMhSd = 0.5,
                        muMhSd = 0.1,
                        psiMhSd = 0.1,
                        ageScaleVal = 1000,
                        positionEps = 1e-5,
                        positionNormalise = TRUE) {

  # Notation:
  # theta are the calibrated ages of ages 1 to n (not necessarily radiocarbon)
  # phi are the outlier indicators (1=TRUE or 0=FALSE) for date i
  # mu,psi are the Compound Poisson-Gamma parameters controlling sedimentation

  # Run data checks ---------------------------------------------------------

  # Run the Bchron check function in case things have gone wrong
  BchronCheck(
    ages = ages,
    ageSds = ageSds,
    positions = positions,
    positionThicknesses = positionThicknesses,
    calCurves = calCurves,
    ids = ids,
    outlierProbs = outlierProbs,
    predictPositions = predictPositions,
    pathToCalCurves = pathToCalCurves,
    artificialThickness = artificialThickness,
    allowOutside = allowOutside,
    iterations = iterations,
    thetaStart = thetaStart,
    burn = burn,
    thin = thin,
    extractDate = extractDate,
    maxExtrap = maxExtrap,
    thetaMhSd = thetaMhSd,
    muMhSd = muMhSd,
    psiMhSd = psiMhSd,
    ageScaleVal = ageScaleVal,
    positionEps = positionEps,
    positionNormalise = positionNormalise,
    type = "Bchronology"
  )

  # Check order of positions ------------------------------------------------
  
  # Check positions are in order
  o <- order(positions)
  if (any(positions[o] != positions)) {
    warning("positions not given in order - re-ordering")
    ages <- ages[o]
    ageSds <- ageSds[o]
    positions <- positions[o]
    positionThicknesses <- positionThicknesses[o]
    calCurves <- calCurves[o]
    ids <- ids[o]
    outlierProbs <- outlierProbs[o]
  }
  
  # Re-normalise positions --------------------------------------------------

  originalPositions <- positions
  originalPositionThicknesses <- positionThicknesses
  originalPredictPositions <- predictPositions
  if (positionNormalise) {
    positionRange <- diff(range(positions))
    positions <- (positions - min(originalPositions)) / positionRange
    positionThicknesses <- positionThicknesses / positionRange
    predictPositionsRescaled <- (predictPositions - min(originalPositions)) / positionRange
  } else {
    predictPositionsRescaled <- predictPositions
  }

  # Check positions don't overlap
  n <- length(ages)
  depthLow <- positions - 0.5 * positionThicknesses
  depthHigh <- positions + 0.5 * positionThicknesses

  # Function to create decent starting positions
  getCurrPositions <- function(pos, posThick, posEps) {
    n <- length(pos)
    badPositions <- TRUE
    while (badPositions) {
      currPositions <- stats::runif(
        n,
        pos - 0.5 * posThick,
        pos + 0.5 * posThick
      )
      do <- order(currPositions)
      diffPosition <- diff(currPositions[do])
      if (all(diffPosition > posEps)) badPositions <- FALSE
    }
    return(currPositions)
  }

  # Calibrate dates ---------------------------------------------------------

  # First thing to do is to calibrate the ages with the two different degrees of freedom
  dfs <- c(1, 100) # corresponding to phi equals 1 and 0 respectively
  x.df1 <- BchronCalibrate(
    ages = ages,
    ageSds = ageSds,
    calCurves = calCurves,
    positions = originalPositions,
    ids = ids,
    pathToCalCurves = pathToCalCurves,
    allowOutside = allowOutside,
    eps = 0,
    dfs = rep(dfs[1], length(ages))
  )
  x.df2 <- BchronCalibrate(
    ages = ages,
    ageSds = ageSds,
    calCurves = calCurves,
    positions = originalPositions,
    ids = ids,
    pathToCalCurves = pathToCalCurves,
    allowOutside = allowOutside,
    eps = 0,
    dfs = rep(dfs[2], length(ages))
  )

  # Finally - need to make sure that all calibrated dates have the same ageGrids
  # This is to avoid the situation where the depth order swaps and you end up
  # with ages that run outside their calibration curve ranges
  # The densities outside this range are set to zero
  range1 <- range(lapply(x.df1, "[", "ageGrid"))
  range2 <- range(lapply(x.df2, "[", "ageGrid"))
  masterAgeGrid1 <- seq(min(range1), max(range1), by = 1)
  masterAgeGrid2 <- seq(min(range2), max(range2), by = 1)
  for (j in 1:length(x.df1)) {
    if (!setequal(x.df1[[j]]$ageGrid, masterAgeGrid1)) {
      currAgeGrid <- x.df1[[j]]$ageGrid
      currDensities <- x.df1[[j]]$densities
      x.df1[[j]]$densities <- rep(0, length(masterAgeGrid1))
      x.df1[[j]]$densities[match(x.df1[[j]]$ageGrid, masterAgeGrid1)] <- currDensities
      x.df1[[j]]$ageGrid <- masterAgeGrid1
    }
    if (!setequal(x.df2[[j]]$ageGrid, masterAgeGrid2)) {
      currAgeGrid <- x.df2[[j]]$ageGrid
      currDensities <- x.df2[[j]]$densities
      x.df2[[j]]$densities <- rep(0, length(masterAgeGrid2))
      x.df2[[j]]$densities[match(x.df2[[j]]$ageGrid, masterAgeGrid2)] <- currDensities
      x.df2[[j]]$ageGrid <- masterAgeGrid2
    }
  }


  # Get starting positions --------------------------------------------------

  # If it has position thicknesses for all then use these to create some current positions

  if (any(positionThicknesses == 0)) {
    if (length(positionThicknesses[duplicated(positions)]) > 0) {
      message("Some positionThicknesses are zero for identical positions. artificialThickness has been added so that the model can attempt to run. If the model still fails then increase the value of artificialThickness further.")
      positionThicknesses[duplicated(positions)] <- positionThicknesses[duplicated(positions)] + artificialThickness
      currPositions <- getCurrPositions(
        positions,
        positionThicknesses, positionEps
      )
    } else {
      currPositions <- getCurrPositions(
        positions,
        positionThicknesses, positionEps
      )
    }
  } else {
    currPositions <- getCurrPositions(
      positions,
      positionThicknesses, positionEps
    )
  }
  do <- order(currPositions)
  diffPosition <- diff(currPositions[do])

  # For all dates we need an offset to enable fast lookup
  # The offset will be the same for all if we're using a master ageGrid
  offset <- rep(-min(x.df1[[1]]$ageGrid), length = n)

  # Useful C functions ------------------------------------------------------

  # C function for truncated random walk
  truncatedWalk <- function(old, sd, low, high) {
    if (isTRUE(all.equal(low, high, tolerance = 1e-4))) {
      return(list(new = low, rat = 1))
    }
    if (high < low) warning("truncatedWalk has lower limit higher than upper limit")

    new <- .C(
      "truncatedWalk",
      as.double(old),
      as.double(sd),
      as.double(low),
      as.double(high),
      as.double(0)
    )[5][[1]]
    rat <- .C(
      "truncatedRat",
      as.double(old),
      as.double(sd),
      as.double(low),
      as.double(high),
      as.double(new),
      as.double(0)
    )[6][[1]]
    if (is.nan(rat)) {
      rat <- 1
    } # Useful for when proposed value and new value are identical
    return(list(new = new, rat = rat))
  }

  # C functions for tweedie
  dtweediep1 <- Vectorize(function(y, p, mu, phi) {
    if (phi < 0) stop("Bad phi parameter value")
    if (mu < 0) stop("Bad mu parameter value")
    return(.C(
      "dtweediep1",
      as.double(y),
      as.double(p),
      as.double(mu),
      as.double(phi),
      as.double(0)
    )[5][[1]])
  })

  # Some C functions to do prediction and interpolation
  predictInterp <- function(alpha,
                            lambda,
                            beta,
                            predictPositions,
                            diffPositionj,
                            currPositionsj,
                            currPositionsjp1,
                            thetaj,
                            thetajp1) {
    return(
      .C(
        "predictInterp",
        as.double(alpha),
        as.double(lambda),
        as.double(beta),
        as.double(predictPositions),
        as.integer(length(predictPositions)),
        as.double(diffPositionj),
        as.double(currPositionsj),
        as.double(currPositionsjp1),
        as.double(thetaj),
        as.double(thetajp1),
        as.double(rep(NA_real_, length(
          predictPositions
        ))),
        NAOK = TRUE
      )[11][[1]]
    )
  }
  predictExtrapUp <- function(alpha,
                              lambda,
                              beta,
                              predictPositions,
                              currPositions1,
                              theta1,
                              maxExtrap,
                              extractDate) {
    return(
      .C(
        "predictExtrapUp",
        as.double(alpha),
        as.double(lambda),
        as.double(beta),
        as.double(predictPositions),
        as.integer(length(predictPositions)),
        as.double(currPositions1),
        as.double(theta1),
        as.integer(maxExtrap),
        as.double(extractDate),
        as.double(rep(NA_real_, length(
          predictPositions
        ))),
        NAOK = TRUE
      )[10][[1]]
    )
  }
  predictExtrapDown <- function(alpha,
                                lambda,
                                beta,
                                predictPositions,
                                currPositionsn,
                                thetan,
                                maxExtrap) {
    return(
      .C(
        "predictExtrapDown",
        as.double(alpha),
        as.double(lambda),
        as.double(beta),
        as.double(predictPositions),
        as.integer(length(predictPositions)),
        as.double(currPositionsn),
        as.double(thetan),
        as.integer(maxExtrap),
        as.double(rep(0, length(
          predictPositions
        ))),
        NAOK = TRUE
      )[9][[1]]
    )
  }
  # End of C functions


  # Starting values ---------------------------------------------------------

  # Written by Nathan McJames - calculate starting values
  if (is.null(thetaStart)) {
    thetaStart <- function(a, b, ageGrids, ageDensities) {
      minbs <- rev(cummin(rev(b)))
      maxas <- cummax(a)
      if (any(minbs < maxas)) {
        stop("Invalid starting values. Use the thetaStart argument to specify valid starting calendar ages")
      } else {
        raw_soln <- rep(NA, length(b))
        for (u in 1:length(b)) {
          currAgeGrid <- ageGrids[[u]]$ageGrid / ageScaleVal
          currDensities <- ageDensities[[u]]$densities
          # Remove previous values from ageGrid
          currAgeGrid <- currAgeGrid[which(!currAgeGrid %in% raw_soln[1:(u - 1)])]
          currDensities <- currDensities[which(!currAgeGrid %in% raw_soln[1:(u - 1)])]
          # Now derive raw (unsorted solutions)
          raw_soln[u] <- sample(
            x = currAgeGrid[currAgeGrid >= maxas[u] & currAgeGrid <= minbs[u]],
            size = 1,
            prob = currDensities[currAgeGrid >= maxas[u] & currAgeGrid <= minbs[u]]
          )
        }
        soln <- sort(raw_soln)
        if (any(diff(soln) < 1e-4)) stop(paste0("Invalid starting values. Use the thetaStart argument to specify valid starting calendar ages. Bchron's initial guess was c(", paste(soln, collapse = ", "), ")"))
        return(sort(soln))
      }
    }
    ageGrids <- lapply(x.df2, "[", "ageGrid")
    ageDensities <- lapply(x.df2, "[", "densities")
    ranges <- do.call(rbind, lapply(ageGrids, range)) / ageScaleVal
    theta <- thetaStart(ranges[, 1], ranges[, 2], ageGrids, ageDensities)
  } else {
    theta <- thetaStart / ageScaleVal
  }
  # Make sure that the thetas are in the same order as the current positions
  theta[do] <- theta

  # Other starting values
  phi <- rep(0, length(theta))
  p <- 1.2
  mu <- abs(stats::rnorm(
    1,
    mean = mean(diff(theta)) / mean(diffPosition),
    sd = 1
  ))
  psi <- abs(stats::rnorm(1, 1, 1))

  # Tranformed values (used for interpolation)
  alpha <- (2 - p) / (p - 1)


  # Storage -----------------------------------------------------------------

  # Storage
  remaining <- (iterations - burn) / thin
  thetaStore <- phiStore <- matrix(ncol = length(theta), nrow = remaining)
  muStore <- psiStore <- vector(length = remaining)
  thetaPredict <- matrix(ncol = length(predictPositions), nrow = remaining)


  # Main iteration loop -----------------------------------------------------

  message("Running Bchronology...\n")
  pb <- utils::txtProgressBar(
    min = 1,
    max = iterations,
    style = 3,
    width = 60,
  )
  for (i in 1:iterations) {
    utils::setTxtProgressBar(pb, i)

    # Update positions --------------------------------------------------------

    if (any(positionThicknesses > 0) &
      i > 0.5 * burn & i %% thin == 0) {
      # Get date order so I can preserve things if they change around
      currPositions <- getCurrPositions(positions, positionThicknesses, positionEps)
      do <- order(currPositions)
      diffPosition <- diff(currPositions[do])
      theta[do] <- sort(theta)
    }

    # Put things in storage ---------------------------------------------------

    # If we're in the right place put things in storage
    if (i > burn & i %% thin == 0) {
      ind <- (i - burn) / thin
      thetaStore[ind, ] <- theta * ageScaleVal
      phiStore[ind, ] <- phi
      muStore[ind] <- mu
      psiStore[ind] <- psi

      # Run interpolation/extrapolation stage
      lambda <- (mu^(2 - p)) / (psi * (2 - p))
      beta <- 1 / (psi * (p - 1) * (mu^(p - 1)))

      # Interpolation and extrapolation -----------------------------------------

      # First interpolation
      for (j in 1:(n - 1)) {
        # Find which positions we need to interpolate for
        depthIndRange <- which(
          predictPositionsRescaled >= currPositions[do[j]] &
            predictPositionsRescaled <= currPositions[do[j + 1]]
        )
        if (length(depthIndRange) > 0) {
          thetaPredict[ind, depthIndRange] <- round(
            predictInterp(
              alpha,
              lambda,
              beta,
              predictPositionsRescaled[depthIndRange],
              diffPosition[j],
              currPositions[do[j]],
              currPositions[do[j + 1]],
              theta[do[j]],
              theta[do[j + 1]]
            ),
            3
          )
        }
      }

      # Extrapolate up to to top depth
      if (any(predictPositionsRescaled < currPositions[1])) {
        depthIndRange <- which(predictPositionsRescaled <= currPositions[1])
        thetaPredict[ind, depthIndRange] <- round(
          predictExtrapUp(
            alpha,
            lambda,
            beta,
            predictPositionsRescaled[depthIndRange],
            currPositions[1],
            theta[1],
            maxExtrap,
            extractDate / ageScaleVal
          ),
          3
        )
      }

      # Extrapolate below bottom depth
      if (any(predictPositionsRescaled >= currPositions[n])) {
        depthIndRange <- which(predictPositionsRescaled >= currPositions[n])
        thetaPredict[ind, depthIndRange] <- round(
          predictExtrapDown(
            alpha,
            lambda,
            beta,
            predictPositionsRescaled[depthIndRange],
            currPositions[n],
            theta[n],
            maxExtrap
          ),
          3
        )
      }

      if (any(is.na(thetaPredict[ind, ]))) {
        warning(
          "NA values in predicted ages. Check you are not extrapolating too far away from dated levels. If you must run this core with these values of predictPositions, set maxExtrap to a larger value (e.g. 1000). "
        )
      }
    }


    # Update theta ------------------------------------------------------------

    for (j in 1:n) {
      lowerLimit <- ifelse(
        j == 1,
        ifelse(x.df1[[do[j]]]$calCurve == "normal",
          extractDate / ageScaleVal,
          min(x.df1[[do[j]]]$ageGrid) / ageScaleVal
        ),
        theta[do[j - 1]] + 0.001
      )
      # upperLimit <- ifelse(j == n, 100000, theta[do[j + 1]] - 0.001)
      upperLimit <- ifelse(j == n,
        max(x.df1[[do[j]]]$ageGrid) / ageScaleVal,
        theta[do[j + 1]] - 0.001
      )

      thetaNewAll <- truncatedWalk(
        theta[do[j]],
        thetaMhSd,
        lowerLimit,
        upperLimit
      )
      thetaNew <- round(thetaNewAll$new, 3)
      # Calculate ratio
      if (phi[do[j]] == 0) {
        currDens <- x.df2[[do[j]]]$densities
      } else {
        currDens <- x.df1[[do[j]]]$densities
      }
      thetaNewMatch <- as.integer(thetaNew * ageScaleVal + offset[do[j]]) +
        1
      thetaNewLogDens <- max(log(currDens[thetaNewMatch]), -1000000,
        na.rm =
          TRUE
      ) # Get rid of NAs in case of moving beyond the calibration curve
      priorNewLogDens <- ifelse(j == 1, 0, log(dtweediep1(
        thetaNew - theta[do[j - 1]],
        p,
        mu * diffPosition[j - 1],
        psi / (diffPosition[j - 1]^(p - 1))
      ))) + ifelse(j == n, 0, log(dtweediep1(
        theta[do[j + 1]] - thetaNew,
        p,
        mu * (diffPosition[j]),
        psi / (diffPosition[j])^(p - 1)
      )))
      thetaMatch <- as.integer(theta[do[j]] * ageScaleVal + offset[do[j]]) +
        1
      thetaLogDens <- max(log(currDens[thetaMatch]), -1000000)
      priorLogDens <- ifelse(j == 1, 0, log(dtweediep1(
        theta[do[j]] - theta[do[j - 1]],
        p,
        mu * (diffPosition[j - 1]),
        psi / (diffPosition[j - 1])^(p - 1)
      ))) + ifelse(j == n, 0, log(dtweediep1(
        theta[do[j + 1]] - theta[do[j]],
        p,
        mu * (diffPosition[j]),
        psi / (diffPosition[j])^(p - 1)
      )))
      logRtheta <- thetaNewLogDens - thetaLogDens + priorNewLogDens - priorLogDens + log(thetaNewAll$rat)
      if (stats::runif(1) < exp(logRtheta)) {
        theta[do[j]] <- thetaNew
      }
    }

    # Update other parameters -------------------------------------------------

    # Update phi
    for (j in 1:n) {
      phiNew <- sample(0:1, 1)
      if (phiNew != phi[do[j]]) {
        if (phi[do[j]] == 0) {
          currDens <- x.df2[[do[j]]]$densities
        } else {
          currDens <- x.df1[[do[j]]]$densities
        }
        thetaMatch <- as.integer(theta[do[j]] * ageScaleVal + offset[j]) +
          1
        thetaLogDens <- max(log(currDens[thetaMatch]), -1000000)
        if (phiNew == 0) {
          newDens <- x.df2[[do[j]]]$densities
        } else {
          newDens <- x.df1[[do[j]]]$densities
        }
        thetaMatch <- as.integer(theta[do[j]] * ageScaleVal + offset[j]) +
          1
        thetaNewLogDens <- max(log(newDens[thetaMatch]), -1000000)

        logRphi <- thetaNewLogDens - thetaLogDens + stats::dbinom(phiNew, 1, outlierProbs[do[j]],
          log =
            TRUE
        ) - stats::dbinom(phi[do[j]], 1, outlierProbs[do[j]], log = TRUE)

        if (stats::runif(1) < exp(logRphi)) {
          phi[do[j]] <- phiNew
        }
      }
    }

    # Update mu
    muNewAll <- truncatedWalk(mu, muMhSd, 1e-3, 1e3)
    muNew <- muNewAll$new

    logRmu <- sum(log(dtweediep1(
      diff(theta[do]),
      p,
      muNew * diffPosition,
      psi / (diffPosition)^(p - 1)
    ))) - sum(log(dtweediep1(
      diff(theta[do]),
      p,
      mu * diffPosition,
      psi / (diffPosition)^(p - 1)
    ))) + log(muNewAll$rat)
    if (stats::runif(1) < exp(logRmu)) {
      mu <- muNew
    }

    # Update psi
    psiNewAll <- truncatedWalk(psi, psiMhSd, 1e-3, 1e3)
    psiNew <- psiNewAll$new

    logRpsi <- sum(log(dtweediep1(
      diff(theta[do]),
      p,
      mu * diffPosition,
      psiNew / (diffPosition)^(p - 1)
    ))) - sum(log(dtweediep1(
      diff(theta[do]),
      p,
      mu * diffPosition,
      psi / (diffPosition)^(p - 1)
    ))) + log(psiNewAll$rat)
    if (stats::runif(1) < exp(logRpsi)) {
      psi <- psiNew
    }
  }


  # Collect up and return ---------------------------------------------------

  # Collect up the input values to return
  inputVals <- list(
    ages = ages,
    ageSds = ageSds,
    positions = originalPositions,
    positionThicknesses = originalPositionThicknesses,
    positionsNormalised = positions,
    positionThicknessesNormalised = positionThicknesses,
    calCurves = calCurves,
    ids = ids,
    outlierProbs = outlierProbs,
    predictPositions = predictPositions,
    pathToCalCurves = pathToCalCurves,
    artificialThickness = artificialThickness,
    iterations = iterations,
    burn = burn,
    thin = thin,
    extractDate = extractDate,
    maxExtrap = maxExtrap,
    thetaMhSd = thetaMhSd,
    muMhSd = muMhSd,
    psiMhSd = psiMhSd,
    ageScaleVal = ageScaleVal,
    positionNormalise = positionNormalise
  )

  # Return everything
  out <- list(
    theta = thetaStore,
    phi = phiStore,
    mu = muStore,
    psi = psiStore,
    thetaPredict = ageScaleVal * thetaPredict,
    predictPositions = originalPredictPositions,
    calAges = x.df2,
    positions = originalPositions,
    extractDate = extractDate,
    ageScaleVal = ageScaleVal,
    positionNormalise = positionNormalise,
    inputVals = inputVals
  )
  class(out) <- "BchronologyRun"
  message("\nRun completed!\n")
  return(out)
}
