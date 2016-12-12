#' Plot output from Bchronology
#' 
#' Plots output from a run of \code{\link{Bchronology}}
#'
#' @param x The object created by \code{\link{Bchronology}}
#' @param dateHeight The height of the date densities plotted in position/depth units
#' @param chronCol The colour of the chronology uncertainty ribbon to be plotted
#' @param chronBorder The colour of the border of the chronology uncertainty ribbon to be plotted
#' @param alpha The credible interval of the chronology run to be plotted. Defaults to 95 percent
#' @param legLoc The location of the plot legend
#' @param ... Other graphical parameters as detailed in \code{\link{par}}
#'
#' @details Creates a simple plot of the chronology output. The height of the date densities in the plots can be manipulated via the \code{dateHeight} argument which is represented in the same units as the positions/depths provided. More detailed plots can be created by manipulating the Bchronology object as required.
#' 
#' @seealso For examples see \code{\link{Bchronology}}. Also \code{\link{BchronCalibrate}}, \code{\link{BchronRSL}}, \code{\link{BchronDensity}}, \code{\link{BchronDensityFast}}
#'
#' @export
plot.BchronologyRun <-
function(x,
         dateHeight = 30,
         chronCol = grDevices::rgb(190/255,190/255,190/255,alpha=0.8),
         chronBorder = grDevices::rgb(190/255,190/255,190/255,alpha=0.8),
         alpha = 0.95,
         legLoc = 'topleft',
         ...) {

  # x contains the output from a run of the Bchronology function

  # Get extra arguments if provided
  ex = list(...)#as.list(substitute(list(...)))[-1L]

  # Get chronology ranges
  chronLow = apply(x$thetaPredict,2,'quantile',probs=(1-alpha)/2)
  chronMed = apply(x$thetaPredict,2,'quantile',probs=0.5)
  chronHigh = apply(x$thetaPredict,2,'quantile',probs=1-(1-alpha)/2)

  yLimits = range(x$predictPositions)
  for(i in 1:length(x$calAges)) {
    yLimits = range(c(yLimits,x$calAges[[i]]$positions))
  }
  yLimits = c(yLimits[1],yLimits[2])
  #dateHeight=0.2*diff(pretty(yLimits))[1]
  yLimits[1] = yLimits[1]-dateHeight
  ex$ylim = rev(yLimits)
  if(is.null(ex$xlim)) ex$xlim = rev(range(c(chronLow,chronHigh)))
  if(is.null(ex$xlab)) ex$xlab = 'Age'
  if(is.null(ex$ylab)) ex$ylab = 'Position'
  if(is.null(ex$las)) ex$las = 1
  if(is.null(ex$main)) ex$main = 'Bchronology plot'
  ex$x = 1
  ex$y = 1
  ex$type = 'n'

  # Modify the arguments if necessary
  args = utils::modifyList(ex, list(...))

  # Create the outline plot
  do.call("plot", args)
  graphics::grid()

  # Add in the dates
  for(i in 1:length(x$calAges)) {
    # First for known points
    if(x$calAges[[i]]$ageSds<5) {
      graphics::points(sum(x$calAges[[i]]$ageGrid*x$calAges[[i]]$densities),x$calAges[[i]]$positions,pch=16)
    } else {
      graphics::polygon(c(min(x$calAges[[i]]$ageGrid),x$calAges[[i]]$ageGrid,max(x$calAges[[i]]$ageGrid)),c(x$calAges[[i]]$positions,x$calAges[[i]]$positions-x$calAges[[i]]$dens*dateHeight/max(x$calAges[[i]]$dens),x$calAges[[i]]$positions),border=NA,col='black')
    }
  }

  # Add in the chronologies
  chronLow = apply(x$thetaPredict,2,'quantile',probs=0.025)
  chronMed = apply(x$thetaPredict,2,'quantile',probs=0.5)
  chronHigh = apply(x$thetaPredict,2,'quantile',probs=0.975)
  graphics::polygon(c(chronLow,rev(chronHigh)),c(x$predictPositions,rev(x$predictPositions)),col=chronCol,border=NA)

  graphics::legend(legLoc,c('Dated positions','95% Chronology CI'),col=c('black',chronCol),pch=15)

}
