plot.BchronCalibratedDates <-
function(x,withPositions=FALSE,xlab='Age (cal years BP)',ylab=ifelse(withPositions,'Position','Density'),pause=FALSE,...) {
  
  # First plot for individual dates
  if(length(x)==1) {
    ag = x[[1]]$ageGrid
    den = x[[1]]$densities
    plot(ag,den,type='l',main=names(x),xlab=xlab,ylab=ylab,...)      
    mtext(paste(x[[1]]$calCurves),side=1,line=4,adj=0,cex=0.6)
  }
  
  # Now for multiple dates without depths
  if(length(x)>1 & withPositions==FALSE) {
    for(i in 1:length(x)) {
      ag = x[[i]]$ageGrid
      den = x[[i]]$densities
      plot(ag,den,type='l',main=names(x)[i],xlab=xlab,ylab=ylab,...)
      mtext(paste(x[[i]]$calCurves),side=1,line=4,adj=0,cex=0.6)
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
    dateHeight=0.2*diff(pretty(ylimits))[1]
    ylimits[1] = ylimits[1]-dateHeight
    
    # Create the plot
    plot(1,1,xlim=rev(xlimits),ylim=rev(ylimits),type="n",main='Calibrated dates by position',,xlab=xlab,ylab=ylab,...)
    for(i in 1:length(x)) {
      polygon(x[[i]]$ageGrid,x[[i]]$positions-x[[i]]$densities*dateHeight/max(x[[i]]$densities),border=NA,col='gray')      
    }
  }
  
}
