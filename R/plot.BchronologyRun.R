plot.BchronologyRun <-
function(x, 
         dateHeight = 30, 
         ...) {
  
  # x contains the output from a run of the Bchronology function
  
  # Get extra arguments if provided
  ex = list(...)#as.list(substitute(list(...)))[-1L]

  # Get chronology ranges
  chronLow = apply(x$thetaPredict,2,'quantile',probs=0.025)
  chronMed = apply(x$thetaPredict,2,'quantile',probs=0.5)
  chronHigh = apply(x$thetaPredict,2,'quantile',probs=0.975)
  
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
  chronCol = grDevices::rgb(190/255,190/255,190/255,alpha=0.8)
  chronLow = apply(x$thetaPredict,2,'quantile',probs=0.025)
  chronMed = apply(x$thetaPredict,2,'quantile',probs=0.5)
  chronHigh = apply(x$thetaPredict,2,'quantile',probs=0.975)
  graphics::polygon(c(chronLow,rev(chronHigh)),c(x$predictPositions,rev(x$predictPositions)),col=chronCol,border=NA)

  graphics::legend('topleft',c('Dated positions','95% Chronology CI'),col=c('black',chronCol),pch=15)
  
}
