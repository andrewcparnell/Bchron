#' Plot output from BchronRSL
#'
#' Plot output from the \code{\link{BchronRSL}} function
#' 
#' @param x An object created by \code{\link{BchronRSL}}
#' @param type One of 'RSL', 'rate', or 'accel'. If RSL produces a plot of RSL esimtates from the model. If rate, produces rate estimates. If accel produces acceleration estimates.
#' @param ... Other arguments to plot, see \code{\link{par}}
#'
#' @seealso \code{\link{BchronCalibrate}}, \code{\link{Bchronology}}, \code{\link{BchronRSL}}, \code{\link{BchronDensity}}, \code{\link{BchronDensityFast}}
#'
#' @return
#' @export
#'
#' @examples
plot.BchronRSLRun <-
function(x, type = c('RSL','rate','accel'), ...) {

  # Match type
  type = match.arg(type, several.ok = TRUE)

  # Get extra arguments if provided
  ex = list(...)#as.list(substitute(list(...)))[-1L]

  if(is.null(ex$xlab)) ex$xlab = 'Age (cal years BP)'

  age.low = apply(x$BchronologyRun$thetaPredict,2,'quantile',probs=0.025)
  age.med = apply(x$BchronologyRun$thetaPredict,2,'quantile',probs=0.5)
  age.high = apply(x$BchronologyRun$thetaPredict,2,'quantile',probs=0.975)
  if(is.null(ex$xlim)) ex$xlim = rev(range(c(age.low,age.high)))

  # Plot raw RSL
  if('RSL' %in% type) {

    ex_RSL = ex

    RSL.low = x$RSLmean-2*x$RSLsd
    RSL.high = x$RSLmean+2*x$RSLsd

    if(is.null(ex_RSL$ylab)) ex_RSL$ylab = 'RSL (m)'
    if(is.null(ex_RSL$ylim)) ex_RSL$ylim = range(c(RSL.low,RSL.high))

    ex_RSL$x = age.med
    ex_RSL$y = x$RSLmean
    ex_RSL$type ='n'

    args = utils::modifyList(ex_RSL, list(...))
    do.call("plot", args)

    for(i in 1:length(x$RSLmean)) {
      agescale = (age.high[i]-age.low[i])/4
      rslscale = x$RSLsd[i]
      graphics::lines(ellipse::ellipse(0,scale=c(agescale,rslscale),centre=c(age.med[i],x$RSLmean[i])),col=grDevices::rgb(0,0,1,0.4))
    }

    xgrid = seq(max(age.low),min(age.high),length=100)/1000

    pred.lines = matrix(NA,ncol=length(xgrid),nrow=nrow(x$samples))
    degmat = matrix(rep(0:(x$degree),length(xgrid)*(x$degree+1)),nrow=length(xgrid),ncol=x$degree+1,byrow=TRUE)
    X.pred = matrix(rep(xgrid-x$const,x$degree+1),ncol=x$degree+1)
    X.pred = (X.pred^degmat)

    for(i in 1:nrow(pred.lines)) {
      pred.lines[i,] = X.pred%*%matrix(x$samples[i,],ncol=1,nrow=x$degree+1)
    }

    pred.med = apply(pred.lines,2,'quantile',probs=0.5)
    pred.low = apply(pred.lines,2,'quantile',probs=0.025)
    pred.high = apply(pred.lines,2,'quantile',probs=0.975)
    graphics::lines(xgrid*1000,pred.med,lwd=2)
    graphics::lines(xgrid*1000,pred.low,lwd=2,lty=2)
    graphics::lines(xgrid*1000,pred.high,lwd=2,lty=2)
  }

  # Plot RSL rate
  if('rate' %in% type) {

    ex_rate = ex

    if(is.null(ex_rate$ylab)) ex_rate$ylab = 'RSL rate (m/kyr)'

    xgrid = seq(max(age.low),min(age.high),length=100)/1000
    ex_rate$x = xgrid*1000

    pred.lines = matrix(NA,ncol=length(xgrid),nrow=nrow(x$samples))
    degmat = matrix(rep(0:(x$degree),length(xgrid)*(x$degree+1)),nrow=length(xgrid),ncol=x$degree+1,byrow=TRUE)
    degmat_rate = matrix(rep(0:(x$degree),length(xgrid)*(x$degree+1)),nrow=length(xgrid),ncol=x$degree+1,byrow=TRUE) - 1
    X.pred = matrix(rep(xgrid-x$const,x$degree+1),ncol=x$degree+1)
    X.pred = -degmat*(X.pred^degmat_rate)

    for(i in 1:nrow(pred.lines)) {
      pred.lines[i,] = X.pred%*%matrix(x$samples[i,],ncol=1,nrow=x$degree+1)
    }
    ex_rate$ylim = range(pred.lines)

    pred.med = apply(pred.lines,2,'quantile',probs=0.5)
    pred.low = apply(pred.lines,2,'quantile',probs=0.025)
    pred.high = apply(pred.lines,2,'quantile',probs=0.975)

    ex_rate$y = pred.med
    ex_rate$type = 'n'

    args = utils::modifyList(ex_rate, list(...))
    do.call("plot", args)

    graphics::lines(xgrid*1000,pred.med,lwd=2)
    graphics::lines(xgrid*1000,pred.low,lwd=2,lty=2)
    graphics::lines(xgrid*1000,pred.high,lwd=2,lty=2)

  }

  # Plot RSL acceleration
  if('accel' %in% type) {
    ex_accel = ex

    if(is.null(ex_accel$ylab)) ex_accel$ylab = 'RSL acceleration (m/kyr/kyr)'

    xgrid = seq(max(age.low),min(age.high),length=100)/1000
    ex_accel$x = xgrid*1000

    pred.lines = matrix(NA,ncol=length(xgrid),nrow=nrow(x$samples))
    degmat = matrix(rep(0:(x$degree),length(xgrid)*(x$degree+1)),nrow=length(xgrid),ncol=x$degree+1,byrow=TRUE)
    degmat_rate = matrix(rep(0:(x$degree),length(xgrid)*(x$degree+1)),nrow=length(xgrid),ncol=x$degree+1,byrow=TRUE) - 1
    degmat_accel = matrix(rep(0:(x$degree),length(xgrid)*(x$degree+1)),nrow=length(xgrid),ncol=x$degree+1,byrow=TRUE) - 2
    X.pred = matrix(rep(xgrid-x$const,x$degree+1),ncol=x$degree+1)
    X.pred = degmat*(degmat-1)*(X.pred^degmat_accel)

    for(i in 1:nrow(pred.lines)) {
      pred.lines[i,] = X.pred%*%matrix(x$samples[i,],ncol=1,nrow=x$degree+1)
    }
    ex_accel$ylim = range(pred.lines)

    pred.med = apply(pred.lines,2,'quantile',probs=0.5)
    pred.low = apply(pred.lines,2,'quantile',probs=0.025)
    pred.high = apply(pred.lines,2,'quantile',probs=0.975)

    ex_accel$y = pred.med
    ex_accel$type = 'n'

    args = utils::modifyList(ex_accel, list(...))
    do.call("plot", args)

    graphics::lines(xgrid*1000,pred.med,lwd=2)
    graphics::lines(xgrid*1000,pred.low,lwd=2,lty=2)
    graphics::lines(xgrid*1000,pred.high,lwd=2,lty=2)

  }

}
