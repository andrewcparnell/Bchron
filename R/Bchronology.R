#' Runs the Compound Poisson-Gamma chronology model of Haslett and Parnell (2008) 
#'
#' Fits a non-parametric chronology model to age/position data according to the Compound Poisson-Gamma model defined by Haslett and Parnell (2008) <DOI:10.1111/j.1467-9876.2008.00623.x>. This version uses a slightly modified Markov chain Monte Carlo fitting algorithm which aims to converge quicker and requires fewer iterations. It also a slightly modified procedure for identifying outliers
#'
#' @param ages A vector of ages (most likely 14C)
#' @param ageSds A vector of 1-sigma values for the ages given above
#' @param positions Position values (e.g. depths) for each age
#' @param positionThicknesses Thickness values for each of the positions. The thickness value should be the full thickness value of the slice. By default set to zero.
#' @param calCurves A vector of values containing either 'intcal13', 'shcal13', 'marine13', or 'normal'. Should be the same length the number of ages supplied. Non-standard calibration curves can be used provided they are supplied in the same format as those previously mentioned and are placed in the same directory, or created via \code{\link{CreateCalCurve}}. Normal indicates a normally-distributed (non-14C) age.
#' @param ids ID names for each age
#' @param outlierProbs A vector of prior outlier probabilities, one for each age. Defaults to 0.01
#' @param predictPositions A vector of positions (e.g. depths) at which predicted age values are required. Defaults to a sequence of length 100 from the top position to the bottom position
#' @param pathToCalCurves File path to where the calibration curves are located. Defaults to the system directory where the 3 standard calibration curves are stored.
#' @param jitterPositions Whether to jigger the positions at startup or not. Default is FALSE but if there are lots of dates at similar depths this may resolve some initialisation problems
#' @param iterations The number of iterations to run the procedure for
#' @param burn The number of starting iterations to discard
#' @param thin The step size for every iteration to keep beyond the burnin
#' @param extractDate The top age of the core. Used for extrapolation purposes so that no extrapolated ages go beyond the top age of the core. Defaults to the current year
#' @param maxExtrap The maximum number of extrapolations to perform before giving up and setting the predicted ages to NA. Useful for when large amounts of extrapolation are required, i.e. some of the predictPositions are a long way from the dated positions
#' @param thetaMhSd The Metropolis-Hastings standard deviation for the age parameters
#' @param muMhSd The Metropolis-Hastings standard deviation for the Compound Poisson-Gamma mean
#' @param psiMhSd The Metropolis-Hastings standard deviation for the Compound Poisson-Gamma scale
#' @param ageScaleVal A scale value for the ages. Bchronology works best when the ages are scaled to be approximately between 0 and 100. The default value is thus 1000 for ages given in years.
#' @param positionScaleVal A scale value for the positions. Bchronology works best when the positions are scaled to be approximately between 0 and 100. The default value is thus 100 for positions given in cm.
#'
#' @details
#' The Bchronology function fits a compound Poisson-Gamma distribution to the increments between the dated levels. This involves a stochastic linear interpolation step where the age gaps are Gamma distributed, and the position gaps are Exponential. Radiocarbon and non-radiocarbon dates (including outliers) are updated within the function also by MCMC.
#'
#' @useDynLib Bchron
#'
#' @return A list of class BchronologyRun which include elements:
#' \itemize{
#'  \item{theta}{The posterior estimated values of the ages}
#'  \item{phi}{The posterior estimated outlier values (1=outlier, 2=not outlier). The means of this parameter give the posterior estimated outlier probabilities}
#'  \item{mu}{The posterior values of the Compound Poisson-Gamma mean}
#'  \item{psi}{The posterior values of the Compound Poisson-Gamma scale}
#'  \item{thetaPredict}{The posterior estimated ages for each of the values in predictPosition}
#'  \item{predictPositions}{The positions at which estimated ages were required}
#'  \item{calAges}{The calibrated ages as output from \code{\link{BchronCalibrate}}}
#'  \item{inputVals}{All of the input values to the Bchronology run}
#' }
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
#' # Run in Bchronology - all but first age uses intcal13
#' GlenOut = Bchronology(ages=Glendalough$ages,ageSds=Glendalough$ageSds,
#'                       calCurves=Glendalough$calCurves,positions=Glendalough$position,
#'                       positionThicknesses=Glendalough$thickness,ids=Glendalough$id,
#'                       predictPositions=seq(0,1500,by=10))
#' 
#' # Summarise it a few different ways
#' summary(GlenOut) # Default is for quantiles of ages at predictPosition values
#' summary(GlenOut, type='convergence') # Check model convergence
#' summary(GlenOut, type='outliers') # Look at outlier probabilities
#' 
#' # Predict for some new positions
#' predictAges = predict(GlenOut, newPositions = c(150,725,1500), newPositionThicknesses=c(5,0,20))
#' 
#' # Plot the output
#' plot(GlenOut,main="Glendalough",xlab='Age (cal years BP)',ylab='Depth (cm)',las=1)
#' }
#' 
#' @export
Bchronology = function(ages,
                       ageSds,
                       positions,
                       positionThicknesses = rep(0,
                                                 length(ages)),
                       calCurves = rep('intcal13',
                                       length(ages)),
                       ids = NULL,
                       outlierProbs = rep(0.01, length(ages)),
                       predictPositions = seq(min(positions),
                                              max(positions),
                                              length  = 100),
                       pathToCalCurves = system.file('data', package = 'Bchron'),
                       jitterPositions = FALSE,
                       iterations = 10000,
                       burn = 2000,
                       thin = 8,
                       extractDate = 1950 - as.numeric(format(Sys.time(), "%Y")),
                       maxExtrap = 1000,
                       thetaMhSd = 0.5,
                       muMhSd = 0.1,
                       psiMhSd = 0.1,
                       ageScaleVal = 1000,
                       positionScaleVal = 100) {

  # Notation:
  # theta are the calibrated ages of ages 1 to n (not necessarily radiocarbon)
  # phi are the outlier indicators (1=TRUE or 0=FALSE) for date i
  # mu,psi are the Compound Poisson-Gamma parameters controlling sedimentation
  
  # Check positions don't overlap
  n = length(ages)
  depthLow = positions - 0.5 * positionThicknesses
  depthHigh = positions + 0.5 * positionThicknesses
  for (i in 2:n)
    if (depthLow[i] - depthHigh[i - 1] < 0 &
        all(positionThicknesses == 0))
      stop(
        'Depth layers identical with no thickness errors - not supported in Bchron. Check thickness errors'
      )
  
  # Re-scale the predicted positions
  predictPositionsRescaled = predictPositions / positionScaleVal
  
  # Check positions are in order
  o = order(positions)
  if (any(positions[o] != positions)) {
    warning("positions not given in order - re-ordering")
    ages = ages[o]
    ageSds = ageSds[o]
    positions = positions[o]
    positionThicknesses = positionThicknesses[o]
    calCurves = calCurves[o]
    ids = ids[o]
    outlierProbs = outlierProbs[o]
  }
  
  # First thing to do is to calibrate the ages with the two different degrees of freedom
  dfs = c(1, 100) # corresponding to phi equals 1 and 0 respectively
  x.df1 = BchronCalibrate(
    ages = ages,
    ageSds = ageSds,
    calCurves = calCurves,
    positions = positions,
    ids = ids,
    pathToCalCurves = pathToCalCurves,
    eps = 0,
    dfs = rep(dfs[1], length(ages))
  )
  x.df2 = BchronCalibrate(
    ages = ages,
    ageSds = ageSds,
    calCurves = calCurves,
    positions = positions,
    ids = ids,
    pathToCalCurves = pathToCalCurves,
    eps = 0,
    dfs = rep(dfs[2], length(ages))
  )

  # Function to find decimal places of positions
  decimalplaces <- Vectorize(function(x) {
    if ((x %% 1) != 0) {
      nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed = TRUE)[[1]][[2]])
    } else {
      return(0)
    }
  })
  
  # Get current positions and their order
  if (jitterPositions) {
    num_decimals = max(decimalplaces(positions / positionScaleVal))
    currPositions = sort(jitter(
      positions / positionScaleVal,
      amount = max(num_decimals / 10, .Machine$double.eps)
    )) # Removed the above due to errors with cores at different age/position scales
  } else {
    currPositions = sort(positions / positionScaleVal)
    if(any(diff(currPositions)==0)) warning(
      'jitterPositions is set to FALSE which means calibration will fail if repeated positions are given'
    )
  }
  diffPosition = diff(currPositions)
  do = order(currPositions)
  
  # For any calibration curves that don't start at 0, we need an offset to enable fast lookup - only supported one so far is normal
  offset = rep(0, length = n)
  for (i in 1:n) {
    offset[i] = ifelse(x.df1[[i]]$calCurve == 'normal', 100, 0)
  }
  
  # Starting values
  theta = vector(length = n)
  # Make sure no theta values are identical
  badThetas = TRUE
  while (badThetas) {
    for (j in 1:n)
      theta[j] = round(stats::rnorm(1, x.df2[[j]]$ageGrid[match(max(x.df2[[j]]$densities), x.df2[[j]]$densities)] /
                                      ageScaleVal, sd = ageSds[j] / ageScaleVal),
                       3)
    theta = sort(theta)
    if (all(diff(theta) != 0))
      badThetas = FALSE
  }
  phi = rep(0, length(theta))
  p = 1.2
  mu = abs(stats::rnorm(
    1,
    mean = mean(diff(theta)) / mean(diffPosition),
    sd = 1
  ))
  psi = abs(stats::rnorm(1, 1, 1))
  
  # Tranformed values (used for interpolation)
  alpha = (2 - p) / (p - 1)
  
  # Storage
  remaining = (iterations - burn) / thin
  thetaStore = phiStore = matrix(ncol = length(theta), nrow = remaining)
  muStore = psiStore = vector(length = remaining)
  thetaPredict = matrix(ncol = length(predictPositions), nrow = remaining)
  
  # Some C functions which are useful
  #################################################
  
  # C function for truncated random walk
  truncatedWalk = function(old, sd, low, high) {
    if (isTRUE(all.equal(low, high, tolerance = 1e-4)))
      return(list(new = low, rat = 1))
    new = .C(
      'truncatedWalk',
      as.double(old),
      as.double(sd),
      as.double(low),
      as.double(high),
      as.double(0)
    )[5][[1]]
    rat = .C(
      'truncatedRat',
      as.double(old),
      as.double(sd),
      as.double(low),
      as.double(high),
      as.double(new),
      as.double(0)
    )[6][[1]]
    if (is.nan(rat))
      rat = 1 # Useful for when proposed value and new value are identical
    return(list(new = new, rat = rat))
  }
  
  # C functions for tweedie
  dtweediep1 = Vectorize(function(y, p, mu, phi) {
    return(.C(
      'dtweediep1',
      as.double(y),
      as.double(p),
      as.double(mu),
      as.double(phi),
      as.double(0)
    )[5][[1]])
  })
  
  # Some C functions to do prediction and interpolation
  predictInterp = function(alpha,
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
        'predictInterp',
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
  predictExtrapUp = function(alpha,
                             lambda,
                             beta,
                             predictPositions,
                             currPositions1,
                             theta1,
                             maxExtrap,
                             extractDate) {
    return(
      .C(
        'predictExtrapUp',
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
  predictExtrapDown = function(alpha,
                               lambda,
                               beta,
                               predictPositions,
                               currPositionsn,
                               thetan,
                               maxExtrap) {
    return(
      .C(
        'predictExtrapDown',
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
  
  # Main iteration loop
  #################################################
  
  pb = utils::txtProgressBar(
    min = 1,
    max = iterations,
    style = 3,
    width = 60,
    title = 'Running Bchronology...'
  )
  for (i in 1:iterations) {
    utils::setTxtProgressBar(pb, i)
    
    if (any(positionThicknesses > 0) &
        i > 0.5 * burn & i %% thin == 0) {
      # Get date order so I can preserve things if they change around
      currPositions = stats::runif(
        n,
        positions / positionScaleVal - 0.5 * positionThicknesses / positionScaleVal,
        positions / positionScaleVal + 0.5 * positionThicknesses / positionScaleVal
      )
      do = order(currPositions)
      diffPosition = diff(currPositions[do])
      theta[do] = sort(theta)
    }
    
    # If we're in the right place put things in storage
    if (i > burn & i %% thin == 0) {
      ind = (i - burn) / thin
      thetaStore[ind,] = theta * ageScaleVal
      phiStore[ind,] = phi
      muStore[ind] = mu
      psiStore[ind] = psi
      
      # Run interpolation/extrapolation stage
      lambda = (mu ^ (2 - p)) / (psi * (2 - p))
      beta = 1 / (psi * (p - 1) * (mu ^ (p - 1)))
      
      # First interpolation
      for (j in 1:(n - 1)) {
        # Find which positions we need to interpolate for
        depthIndRange = which(
          predictPositionsRescaled >= currPositions[do[j]] &
            predictPositionsRescaled <= currPositions[do[j + 1]]
        )
        if (length(depthIndRange) > 0) {
          thetaPredict[ind, depthIndRange] = round(
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
        depthIndRange = which(predictPositionsRescaled <= currPositions[1])
        thetaPredict[ind, depthIndRange] = round(
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
        depthIndRange = which(predictPositionsRescaled >= currPositions[n])
        thetaPredict[ind, depthIndRange] = round(
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
      
      if (any(is.na(thetaPredict[ind,])))
        warning(
          "NA values in predicted ages. Check you are not extrapolating too far away from dated levels. If you must run this core with these values of predictPositions, set maxExtrap to a larger value (e.g. 1000). "
        )
      
    }
    
    # Update theta
    for (j in 1:n) {
      thetaNewAll = truncatedWalk(
        theta[do[j]],
        thetaMhSd,
        ifelse(
          j == 1,
          ifelse(x.df1[[j]]$calCurve == 'normal', extractDate / ageScaleVal, 0),
          theta[do[j - 1]] + 0.001
        ),
        ifelse(j == n, 100000, theta[do[j + 1]] - 0.001)
      )
      thetaNew = round(thetaNewAll$new, 3)
      # Calculate ratio
      if (phi[do[j]] == 0) {
        currDens = x.df2[[do[j]]]$densities
      } else {
        currDens = x.df1[[do[j]]]$densities
      }
      thetaNewMatch = as.integer(thetaNew * ageScaleVal + offset[do[j]]) +
        1
      thetaNewLogDens = max(log(currDens[thetaNewMatch]),-1000000, na.rm =
                              TRUE) # Get rid of NAs in case of moving beyond the calibration curve
      priorNewLogDens = ifelse(j == 1, 0, log(dtweediep1(
        thetaNew - theta[do[j - 1]],
        p,
        mu * diffPosition[j - 1],
        psi / (diffPosition[j - 1] ^ (p - 1))
      ))) + ifelse(j == n, 0, log(dtweediep1(
        theta[do[j + 1]] - thetaNew,
        p,
        mu * (diffPosition[j]),
        psi / (diffPosition[j]) ^ (p - 1)
      )))
      thetaMatch = as.integer(theta[do[j]] * ageScaleVal + offset[do[j]]) +
        1
      thetaLogDens = max(log(currDens[thetaMatch]),-1000000)
      priorLogDens = ifelse(j == 1, 0, log(dtweediep1(
        theta[do[j]] - theta[do[j - 1]],
        p,
        mu * (diffPosition[j - 1]),
        psi / (diffPosition[j - 1]) ^ (p - 1)
      ))) + ifelse(j == n, 0, log(dtweediep1(
        theta[do[j + 1]] - theta[do[j]],
        p,
        mu * (diffPosition[j]),
        psi / (diffPosition[j]) ^ (p - 1)
      )))
      
      logRtheta = thetaNewLogDens - thetaLogDens + priorNewLogDens - priorLogDens + log(thetaNewAll$rat)
      if (stats::runif(1) < exp(logRtheta))
        theta[do[j]] = thetaNew
    }
    
    # Update phi
    for (j in 1:n) {
      phiNew = sample(0:1, 1)
      if (phiNew != phi[do[j]]) {
        if (phi[do[j]] == 0) {
          currDens = x.df2[[do[j]]]$densities
        } else {
          currDens = x.df1[[do[j]]]$densities
        }
        thetaMatch = as.integer(theta[do[j]] * ageScaleVal + offset[j]) +
          1
        thetaLogDens = max(log(currDens[thetaMatch]),-1000000)
        if (phiNew == 0) {
          newDens = x.df2[[do[j]]]$densities
        } else {
          newDens = x.df1[[do[j]]]$densities
        }
        thetaMatch = as.integer(theta[do[j]] * ageScaleVal + offset[j]) +
          1
        thetaNewLogDens = max(log(newDens[thetaMatch]),-1000000)
        
        logRphi = thetaNewLogDens - thetaLogDens + stats::dbinom(phiNew, 1, outlierProbs[do[j]], log =
                                                                   TRUE) - stats::dbinom(phi[do[j]], 1, outlierProbs[do[j]], log = TRUE)
        
        if (stats::runif(1) < exp(logRphi))
          phi[do[j]] = phiNew
      }
    }
    
    # Update mu
    muNewAll = truncatedWalk(mu, muMhSd, 1e-4, 1e3)
    muNew = muNewAll$new
    
    logRmu = sum(log(dtweediep1(
      diff(theta[do]),
      p,
      muNew * diffPosition,
      psi / (diffPosition) ^ (p - 1)
    ))) - sum(log(dtweediep1(
      diff(theta[do]),
      p,
      mu * diffPosition,
      psi / (diffPosition) ^ (p - 1)
    ))) + log(muNewAll$rat)
    if (stats::runif(1) < exp(logRmu))
      mu = muNew
    
    # Update psi
    psiNewAll = truncatedWalk(psi, psiMhSd, 1e-4, 1e3)
    psiNew = psiNewAll$new
    
    logRpsi = sum(log(dtweediep1(
      diff(theta[do]),
      p,
      mu * diffPosition,
      psiNew / (diffPosition) ^ (p - 1)
    ))) - sum(log(dtweediep1(
      diff(theta[do]),
      p,
      mu * diffPosition,
      psi / (diffPosition) ^ (p - 1)
    ))) + log(psiNewAll$rat)
    if (stats::runif(1) < exp(logRpsi))
      psi = psiNew
    
  }
  
  # Collect up the input values to return
  inputVals = list(ages = ages,
                   ageSds = ageSds,
                   positions = positions,
                   positionThicknesses = positionThicknesses,
                   calCurves = calCurves,
                   ids = ids,
                   outlierProbs = outlierProbs,
                   predictPositions = predictPositions,
                   pathToCalCurves = pathToCalCurves,
                   jitterPositions = jitterPositions,
                   iterations = iterations,
                   burn = burn,
                   thin = thin,
                   extractDate = extractDate,
                   maxExtrap = maxExtrap,
                   thetaMhSd = thetaMhSd,
                   muMhSd = muMhSd,
                   psiMhSd = psiMhSd,
                   ageScaleVal = ageScaleVal,
                   positionScaleVal = positionScaleVal)
  
  # Return everything
  out = list(
    theta = thetaStore,
    phi = phiStore,
    mu = muStore,
    psi = psiStore,
    thetaPredict = ageScaleVal * thetaPredict,
    predictPositions = predictPositions,
    calAges = x.df2,
    positions = positions,
    extractDate = extractDate,
    ageScaleVal = ageScaleVal,
    positionScaleVal = positionScaleVal,
    inputVals = inputVals
  )
  class(out) = 'BchronologyRun'
  return(out)
  
}