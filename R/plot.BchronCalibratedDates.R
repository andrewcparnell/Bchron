plot.BchronCalibratedDates <-
function(x,
         withPositions=FALSE,
         pause=FALSE,
         dateHeight = 30,
         ...) {

  # Get extra arguments if provided
  ex = list(...)#as.list(substitute(list(...)))[-1L]

  if(is.null(ex$xlab)) ex$xlab = 'Age (cal years BP)'
  if(is.null(ex$ylab)) ex$ylab = ifelse(withPositions,'Position','Density')

  # First plot for individual dates
  if(length(x)==1) {
    ag = x[[1]]$ageGrid
    den = x[[1]]$densities
    ex$x = ag
    ex$y = den
    ex$type = 'l'
    if(is.null(ex$main)) ex$main = names(x)
    args = utils::modifyList(ex, list(...))
    do.call("plot", args)
    #graphics::plot(ag,den,type='l',main=names(x),xlab=xlab,ylab=ylab,...)
    graphics::mtext(paste(x[[1]]$calCurves),side=1,line=4,adj=0,cex=0.6)
  }

  # Now for multiple dates without depths
  if(length(x)>1 & withPositions==FALSE) {
    for(i in 1:length(x)) {
      ex_curr = ex
      ag = x[[i]]$ageGrid
      den = x[[i]]$densities
      ex_curr$x = ag
      ex_curr$y = den
      ex_curr$type = 'l'
      if(is.null(ex_curr$main)) ex_curr$main = names(x)[i]
      args = utils::modifyList(ex_curr, list(...))
      do.call("plot", args)
      graphics::mtext(paste(x[[i]]$calCurves),side=1,line=4,adj=0,cex=0.6)
      if(pause) if(i<length(x)) readline('Hit Enter for next plot...')
    }
  }

  # Finally for multiple dates with depths
  if(length(x)>1 & withPositions==TRUE) {
    xlimits = NULL
    ylimits = NULL
    for(i in 1:length(x)) {
      xlimits = range(c(xlimits,x[[i]]$ageGrid))
      ylimits = range(c(ylimits,x[[i]]$positions))
    }
    #dateHeight=0.2*diff(pretty(ylimits))[1]
    ylimits[1] = ylimits[1]-dateHeight
    if(is.null(ex$xlim)) ex$xlim = rev(xlimits)
    if(is.null(ex$ylim)) ex$ylim = rev(ylimits)
    ex$x = ex$y = 1
    ex$type = 'n'
    if(is.null(ex$main)) ex$main = 'Calibrated dates by position'
    args = utils::modifyList(ex, list(...))
    do.call("plot", args)

    for(i in 1:length(x)) {
      graphics::polygon(x[[i]]$ageGrid,x[[i]]$positions-x[[i]]$densities*dateHeight/max(x[[i]]$densities),border=NA,col='gray')
    }
  }

}
