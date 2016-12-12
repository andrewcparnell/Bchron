#' Summarise a BchronRSL run
#'
#' Summarise a \code{\link{BchronRSL}} run
#'
#' @param object The output from a run of \code{\link{BchronRSL}}
#' @param type One of 'parameters', 'RSL', 'rate', or 'accel'. If parameters, provides posterior credibility intervals of the regression coefficients. If RSL provides predicted RSL values. If rate, provides rate estimates. If accel provides acceleration estimates.
#' @param age_grid An optional age grid for computing RSL, rate, or acceleration estimates. If not provided uses the age range of the Bchronology run
#' @param ... Other arugments to functions (not currently implemented)
#'
#' @seealso \code{\link{BchronCalibrate}}, \code{\link{Bchronology}}, \code{\link{BchronRSL}}, \code{\link{BchronDensity}}, \code{\link{BchronDensityFast}}
#'
#' @export
summary.BchronRSLRun <-
function(object, type = c('parameters', 'RSL', 'rate', 'accel'), age_grid = NULL, ...) {
  cat('Posterior Medians with 95% credible intervals...\n')

  # Match type
  type = match.arg(type, several.ok = TRUE)

  if('parameters' %in% type) {
    cat('Power Lower Median Upper \n')
    pow.names = c('mean','linear','quadratic','cubic','quartic','quintic')
    for(j in 1:(object$degree+1)) {
      cat(pow.names[j],round(stats::quantile(object$samples[,j],probs=c(0.025,0.5,0.975)),4),'\n')
    }
  }

  # Provide an age grid if not provided
  if(is.null(age_grid)) {
    age_low = apply(object$BchronologyRun$thetaPredict,2,stats::quantile,probs=0.025)
    age_med = apply(object$BchronologyRun$thetaPredict,2,stats::quantile,probs=0.5)
    age_high = apply(object$BchronologyRun$thetaPredict,2,stats::quantile,probs=0.975)
    age_grid = seq(max(age_low),min(age_high),length=100)
  }
  age_grid_use = age_grid / 1000
  pred.lines = matrix(NA,ncol=length(age_grid_use),nrow=nrow(object$samples))

  if('RSL' %in% type) {
    degmat = matrix(rep(0:(object$degree),length(age_grid_use)*(object$degree+1)),nrow=length(age_grid_use),ncol=object$degree+1,byrow=TRUE)
    X.pred = matrix(rep(age_grid_use-object$const,object$degree+1),ncol=object$degree+1)
    X.pred = (X.pred^degmat)

    for(i in 1:nrow(pred.lines)) {
      pred.lines[i,] = X.pred%*%matrix(object$samples[i,],ncol=1,nrow=object$degree+1)
    }

    pred.med = apply(pred.lines,2,stats::quantile,probs=0.5)
    pred.low = apply(pred.lines,2,stats::quantile,probs=0.025)
    pred.high = apply(pred.lines,2,stats::quantile,probs=0.975)
    df = data.frame('Age' = age_grid,
                    'RSL_2.5' = pred.low,
                    'RSL_50' = pred.med,
                    'RSL_97.5' = pred.high)
    print(df)
  }

  if('rate' %in% type) {
    degmat = matrix(rep(0:(object$degree),length(age_grid_use)*(object$degree+1)),nrow=length(age_grid_use),ncol=object$degree+1,byrow=TRUE)
    degmat_rate = matrix(rep(0:(object$degree),length(age_grid_use)*(object$degree+1)),nrow=length(age_grid_use),ncol=object$degree+1,byrow=TRUE) - 1
    X.pred = matrix(rep(age_grid_use-object$const,object$degree+1),ncol=object$degree+1)
    X.pred = -degmat*(X.pred^degmat_rate)

    for(i in 1:nrow(pred.lines)) {
      pred.lines[i,] = X.pred%*%matrix(object$samples[i,],ncol=1,nrow=object$degree+1)
    }

    pred.med = apply(pred.lines,2,stats::quantile,probs=0.5)
    pred.low = apply(pred.lines,2,stats::quantile,probs=0.025)
    pred.high = apply(pred.lines,2,stats::quantile,probs=0.975)

    df = data.frame('Age' = age_grid,
                    'RSL_rate_2.5' = pred.low,
                    'RSL_rate_50' = pred.med,
                    'RSL_rate_97.5' = pred.high)
    print(df)
  }

  if('accel' %in% type) {
    degmat = matrix(rep(0:(object$degree),length(age_grid_use)*(object$degree+1)),nrow=length(age_grid_use),ncol=object$degree+1,byrow=TRUE)
    degmat_rate = matrix(rep(0:(object$degree),length(age_grid_use)*(object$degree+1)),nrow=length(age_grid_use),ncol=object$degree+1,byrow=TRUE) - 1
    degmat_accel = matrix(rep(0:(object$degree),length(age_grid_use)*(object$degree+1)),nrow=length(age_grid_use),ncol=object$degree+1,byrow=TRUE) - 2
    X.pred = matrix(rep(age_grid_use-object$const,object$degree+1),ncol=object$degree+1)
    X.pred = degmat*(degmat-1)*(X.pred^degmat_accel)

    for(i in 1:nrow(pred.lines)) {
      pred.lines[i,] = X.pred%*%matrix(object$samples[i,],ncol=1,nrow=object$degree+1)
    }

    pred.med = apply(pred.lines,2,stats::quantile,probs=0.5)
    pred.low = apply(pred.lines,2,stats::quantile,probs=0.025)
    pred.high = apply(pred.lines,2,stats::quantile,probs=0.975)

    df = data.frame('Age' = age_grid,
                    'RSL_accel_2.5' = pred.low,
                    'RSL_accel_50' = pred.med,
                    'RSL_accel_97.5' = pred.high)
    print(df)

  }

}
