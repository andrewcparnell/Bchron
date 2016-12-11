#' Plot output from BchronDensity
#'
#' @param x Output from \code{\link{BchronDensity}}
#' @param plotDates Whether to plot the individual calibrated dates
#' @param plotRawSum Whether to plot the raw sum of the probability distributions
#' @param plotPhase Whether to plot the phase values
#' @param phaseProb The probability value for the phase identification
#' @param ... Other graphical commands. See \code{\link{par}}
#'
#' @seealso See  \code{\link{BchronDensity}} for examples, also \code{\link{Bchronology}}, \code{\link{BchronRSL}}, and \code{\link{BchronDensityFast}} for a faster approximate version of this function
#'
#' @return
#' @export
#'
#' @examples
plot.BchronDensityRun <-
function(x,plotDates=TRUE,plotRawSum=FALSE,plotPhase=TRUE,phaseProb=0.95,...) {

  # x is the object with everything in it
  # Create a grid over which to plot - the range of the dates plus 10% each side
  n = length(x$calAges)
  thetaRange = range(x$calAges[[1]]$ageGrid)
  for(i in 2:n) thetaRange = range(c(thetaRange,x$calAges[[i]]$ageGrid))
  dateGrid = seq(round(thetaRange[1]*0.9,3),round(thetaRange[2]*1.1,3),length=1000)

  # Create some Gaussian basis functions to use
  gauss <- function(x, mu, sig) {
    # Gaussian-shaped function
    u <- (x - mu) / sig
    y <- exp(- u * u / 2)
    y }

  gbase <- function(x, mus) {
    # Construct Gaussian basis
    sig <- (mus[2] - mus[1]) / 2
    G <- outer(x, mus, gauss, sig)
    G }

  Gstar = gbase(dateGrid,x$mu)

  # Create the densities
  dens = vector(length=length(dateGrid))
  for(i in 1:nrow(x$p)) {
    dens = dens + Gstar%*%x$p[i,]
  }
  densFinal = dens/sum(dens)

  graphics::plot(dateGrid,densFinal,type='l',ylab='Density',ylim=range(c(0,densFinal)),...)

  if(plotDates) {
    # Plot the individual dates
    yHeight=graphics::par('usr')[4]
    myCol = grDevices::rgb(190/255,190/255,190/255,0.4)
    for(i in 1:n) {
      graphics::polygon(x$calAges[[i]]$ageGrid,0.3*yHeight*x$calAges[[i]]$densities/max(x$calAges[[i]]$densities),col=myCol,border=NA)
    }
  }

  if(plotRawSum) {
    yHeight=graphics::par('usr')[4]
    thetaRange = range(x$calAges[[1]]$ageGrid)
    for(i in 2:n) thetaRange = range(c(thetaRange,x$calAges[[i]]$ageGrid))
    dateGrid = seq(round(thetaRange[1]*0.9,0),round(thetaRange[2]*1.1,0),by=1)
    sumDens = rep(0,length(dateGrid))
    for(i in 1:n) {
      matchRows = match(x$calAges[[i]]$ageGrid,dateGrid)
      sumDens[matchRows] = sumDens[matchRows]+x$calAges[[i]]$densities
      if(any(is.na(matchRows))) stop()
    }
    graphics::lines(dateGrid,sumDens*yHeight/max(sumDens),col='red')
  }

  if(plotPhase) {
    phases = hdr(x, prob = phaseProb)
    for(i in 1:length(phases)) {
      lines(c(phases[[i]][1],phases[[i]][2]), c(0,0), lwd = 3)
    }
  }

}
