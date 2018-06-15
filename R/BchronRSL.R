#' Relative sea level rate (RSL) estimation
#'
#' @param BchronologyRun Output from a run of \code{\link{Bchronology}}
#' @param RSLmean A vector of RSL mean estimates of the same length as the number of predictPositions given to the \code{\link{Bchronology}} function
#' @param RSLsd A vector RSL standard deviations of the same length as the number of predictPositions given to the \code{\link{Bchronology}} function
#' @param degree The degree of the polynomial regression: linear=1 (default), quadratic=2, etc. Supports up to degree 5, though this will depend on the data given
#' @param iterations The number of MCMC iterations to run
#' @param burn The number of starting iterations to discard
#' @param thin The step size of iterations to discard
#'
#' @details 
#' This function fits an errors-in-variables regression model to relative sea level (RSL) data. An errors-in-variables regression model allows for uncertainty in the explanatory variable, here the age of sea level data point. The algorithm is more fully defined in the reference below
#' 
#' @return An object of class BchronRSLRun with elements
#' itemize{
#' \item{BchronologyRun}{The output from the run of \code{\link{Bchronology}}}
#' \item{samples}{The posterior samples of the regression parameters}
#' \item{degree}{The degree of the polynomial regression}
#' \item{RSLmean}{The RSL mean values given to the function}
#' \item{RSLsd}{The RSL standard deviations as given to the function}
#' \item{const}{The mean of the predicted age values. Used to standardise the design matrix and avoid computational issues}
#' }
#' 
#' @references 
#' Andrew C. Parnell and W. Roland Gehrels (2013) 'Using chronological models in late holocene sea level reconstructions from salt marsh sediments' In: I. Shennan, B.P. Horton, and A.J. Long (eds). Handbook of Sea Level Research. Chichester: Wiley
#' 
#' @seealso \code{\link{BchronCalibrate}}, \code{\link{Bchronology}}, \code{\link{BchronDensity}}, \code{\link{BchronDensityFast}} 
#' @export
#'
#' @examples
#' \donttest{
#' # Load in data
#' data(TestChronData)
#' data(TestRSLData)
#' 
#' # Run through Bchronology
#' RSLrun = Bchronology(ages=TestChronData$ages,
#'                      ageSds=TestChronData$ageSds,
#'                      positions=TestChronData$position,
#'                      positionThicknesses=TestChronData$thickness,
#'                      ids=TestChronData$id,
#'                      calCurves=TestChronData$calCurves,
#'                      predictPositions=TestRSLData$Depth,
#'                      jitterPositions = TRUE)
#' 
#' # Now run through BchronRSL
#' RSLrun2 = BchronRSL(RSLrun,RSLmean=TestRSLData$RSL,RSLsd=TestRSLData$Sigma,degree=3)
#' 
#' # Summarise it
#' summary(RSLrun2)
#' 
#' # Plot it
#' plot(RSLrun2)
#' }
BchronRSL = function(BchronologyRun,RSLmean,RSLsd,degree=1,iterations=10000,burn=2000,thin=8) {
  UseMethod('BchronRSL')
}

#' @export
BchronRSL.BchronologyRun = function(BchronologyRun,RSLmean,RSLsd,degree=1,iterations=10000,burn=2000,thin=8) {
  if(degree>5) stop('Degree not supported')
  remaining=(iterations-burn)/thin
  betaStore = matrix(NA,ncol=degree+1,nrow=remaining)
  y = matrix(RSLmean,ncol=1)
  Q = diag(1/((RSLsd)^2))
  N = nrow(y)
  chrons = BchronologyRun$thetaPredict/1000
  whichrows = sample(1:nrow(chrons),iterations,replace=TRUE)
  degmat = matrix(rep(0:(degree),ncol(chrons)*N),nrow=N,ncol=degree+1,byrow=TRUE)
  const = mean(as.matrix(chrons))
  pb = utils::txtProgressBar(min = 1, max = iterations, style = 3,width=60,title='Running BchronRSL')
  for(i in 1:iterations) {
    utils::setTxtProgressBar(pb, i)
    if(i%%20==0 | i==1) {
      currchron = chrons[whichrows[i],] - const
      X = matrix(rep(as.numeric(currchron),degree+1),ncol=degree+1)
      X = X^degmat
    }
    # Sample a beta
    if(i%%thin==0 & i>burn) {
      betaStore[(i-burn)/thin,] = matrix(MASS::mvrnorm(1,solve(t(X)%*%Q%*%X,t(X)%*%Q%*%y),solve(t(X)%*%Q%*%X)),ncol=1)
    }
  }
    
  out = list(BchronologyRun=BchronologyRun,samples=betaStore,degree=degree,RSLmean=RSLmean,RSLsd=RSLsd,const=const)
  class(out) = 'BchronRSLRun'
  return(out)
  
}
