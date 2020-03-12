#' Predict ages of other positions for a BchronologyRun object
#'
#' This function will predict the ages of new positions (usually depths) based on a previous run of the function \code{\link{Bchronology}}. It will also allow for thickness uncertainties to be included in the resulting ages, for example when the age of a particular event is desired
#'
#' @param object Output from a run of \code{\link{Bchronology}}
#' @param newPositions A vector of new positions at which to find ages
#' @param newPositionThicknesses A vector of thicknesses for the above positions. Must be the same length as \code{newPositions}
#' @param maxExtrap The maximum new of extrapolation attempts. It might be worth increasing this if you are extrapolating a long way from the other dated positions
#' @param ... Other arguments to predict (not currently supported)
#'
#' @seealso \code{\link{BchronCalibrate}}, \code{\link{Bchronology}} \code{\link{BchronRSL}}, \code{\link{BchronDensity}}, \code{\link{BchronDensityFast}}
#'
#' @useDynLib Bchron
#'
#' @return A matrix of dimension num_samples by num_positions so that each row represents a set of monotonic sample predicted ages
#' @export
predict.BchronologyRun = function(object,
                                  newPositions,
                                  newPositionThicknesses = NULL,
                                  maxExtrap = 500,
                                  ...) {
  # This function takes a BchronologyRun object and produces new age predictions based on the values given in newPositions. If thicknesses are given as well it will produce values for that too
  # The output is a matrix of values where the number of rows is the number of stored iterations in object, and the number of columns is the number of positions given (averaged over thicknesses)
  
  # Check that, if thicknesses are given, they are the same length as newPositions
  if (!is.null(newPositionThicknesses)) {
    if (length(newPositionThicknesses) != length(newPositions))
      stop("newPositionThicknesses and newPositions must be of same length")
  }
  
  # Need some of the C functions for prediction
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
        as.double(rep(0, length(
          predictPositions
        )))
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
        as.double(rep(0, length(
          predictPositions
        )))
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
        )))
      )[9][[1]]
    )
  }
  
  # Get some useful things to start of
  nSamples = length(object$mu)
  out = matrix(ncol = length(newPositions), nrow = nSamples)
  p = 1.2
  alpha = (2 - p) / (p - 1)
  originalNewPositions = newPositions
  if(object$positionNormalise) {
    positionRange = diff(range(object$positions))
    oldPositions = (object$positions - min(object$positions)) / positionRange
    diffPosition = diff(oldPositions)
    newPositions = (newPositions - min(object$positions)) / positionRange
    if(!is.null(newPositionThicknesses))
       newPositionThicknesses = newPositionThicknesses / positionRange
  } else {
    oldPositions = object$positions
    diffPosition = diff(oldPositions)
  }
  
  # Now loop through all the values in newPositions
  pb = utils::txtProgressBar(
    min = 1,
    max = nSamples,
    style = 3,
    width = 60,
    title = 'Calculating predictions... '
  )
  n = length(object$positions)
  for (i in 1:nSamples) {
    utils::setTxtProgressBar(pb, i)
    
    if (is.null(newPositionThicknesses)) {
      currPosition = newPositions
    } else {
      currPosition = sort(
        stats::runif(
          length(newPositions),
          newPositions - 0.5 * newPositionThicknesses,
          newPositions + 0.5 * newPositionThicknesses
        )
      )
    }
    
    # Get sedimentation rate parameters
    lambda = (object$mu[i] ^ (2 - p)) / (object$psi[i] * (2 - p))
    beta = 1 / (object$psi[i] * (p - 1) * (object$mu[i] ^ (p - 1)))
    theta = object$theta[i, ] / object$ageScaleVal
    
    # First interpolation
    for (j in 1:n) {
      # Find which positions we need to interpolate for
      depthIndRange = which(currPosition >= oldPositions[j] &
                              currPosition <= oldPositions[j + 1])
      if (length(depthIndRange) > 0) {
        out[i, depthIndRange] = round(
          predictInterp(
            alpha,
            lambda,
            beta,
            currPosition[depthIndRange],
            diffPosition[j],
            oldPositions[j],
            oldPositions[j + 1],
            theta[j],
            theta[j + 1]
          ),
          3
        )
      }
      # End of j loop
    }
    
    # Extrapolation up
    if (any(currPosition < oldPositions[1])) {
      depthIndRange = which(currPosition <= oldPositions[1])
      out[i, depthIndRange] = round(
        predictExtrapUp(
          alpha,
          lambda,
          beta,
          currPosition[depthIndRange],
          oldPositions[1],
          theta[1],
          maxExtrap,
          object$extractDate / object$ageScaleVal
        ),
        3
      )
    }
    
    # Extrapolate down
    if (any(currPosition >= oldPositions[n])) {
      depthIndRange = which(currPosition >= oldPositions[n])
      out[i, depthIndRange] = round(
        predictExtrapDown(
          alpha,
          lambda,
          beta,
          currPosition[depthIndRange],
          oldPositions[n],
          theta[n],
          maxExtrap
        ),
        3
      )
    }
    
    # End of i loop
  }
  
  colnames(out) = paste0('Pos', originalNewPositions)
  return(out * object$ageScaleVal)
}