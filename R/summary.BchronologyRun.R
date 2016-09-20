summary.BchronologyRun <-
function(object,type=c('quantiles','outliers','convergence','sed_rate','acc_rate'),probs=c(0.025,0.1,0.5,0.9,0.975), useExisting = TRUE, ..., digits = max(3, getOption("digits")-3)) {
  type = match.arg(type)

  switch(type,
    # Give a list of quantiles for each depth and Show outlier probabilities
    quantiles = {
      chronSummary <- data.frame(cbind(object$predictPositions,round(t(apply(object$thetaPredict,2,stats::quantile,probs)),3)))
      colnames(chronSummary) <- c('Depth',paste(probs*100,'%',sep=''))
      cat('Quantiles of predicted ages by depth: \n')
      print(round(chronSummary, digits=digits),row.names=FALSE)
      invisible(chronSummary)
    },
    outliers = {
      cat('Posterior outlier probability by date: \n')
      outprob <- data.frame(names(object$calAges),colMeans(object$phi))
      colnames(outprob) <- c('Date','OutlierProb')
      print(outprob,row.names=FALSE)
      invisible(outprob)
    },
    convergence = {
      cat('Convergence check (watch for too many small p-values): \n')
      pars <- cbind(object$theta,object$phi,object$mu,object$psi)
      n <- ncol(object$theta)
      colnames(pars) <- c(names(object$calAges),paste('Outlier',1:n),'RateMean','RateVar')
      geweke <- coda::geweke.diag(pars)[[1]]
      geweke[is.nan(geweke)] <- 0
      pvals <- data.frame(sort(round(c(stats::pnorm(geweke[geweke<0]),1-stats::pnorm(geweke[geweke>0])),5)))
      colnames(pvals) <- 'p-value'
      print(pvals)
      invisible(pvals)
    },
    sed_rate = {
      cat('Sedimentation rate (time units per position unit): \n')
      chrons = object$thetaPredict
      position_grid = object$predictPositions
      if(useExisting) cat('Note: assumes existing predictPositions are on a regular unit grid. If this is not the case set useExisting = FALSE \n')
      diff_position_grid = diff(position_grid)
      if(!isTRUE(all.equal(diff_position_grid, rep(1, length(diff_position_grid))))) if(useExisting) warning('predictPositions does not seem to be unit spaced. If not done alreday, set useExisting = FALSE')
      if(!useExisting) {
        position_grid = seq(min(object$predictPositions), max(object$predictPositions), by = 1)
        chrons = predict.BchronologyRun(object, newPositions = position_grid)
    }
      sed_rate = apply(chrons, 1, 'diff')
      sed_rate_ci = cbind(position_grid[-1], t(apply(sed_rate, 1, 'quantile', probs = probs)))
      colnames(sed_rate_ci)[1] = 'position_grid'
      print(round(sed_rate_ci, digits=digits),row.names=FALSE)
      invisible(sed_rate_ci)
    },
    acc_rate = {
      cat('Accumulation rate (position units per time unit): \n')
      chrons = object$thetaPredict
      age_grid = seq(quantile(chrons, 0.05), quantile(chrons, 0.95), by = 1)
      my_fun = function(x) stats::approx(x, y = object$predictPositions, xout = age_grid, rule = 2)$y
      position_interp = t(apply(chrons, 1, my_fun))
      acc_rate = apply(position_interp, 1, 'diff')
      acc_rate_ci = cbind(age_grid[-1], t(apply(acc_rate, 1, 'quantile', probs = probs)))
      colnames(acc_rate_ci)[1] = 'age_grid'
      print(round(acc_rate_ci, digits=digits),row.names=FALSE)
      invisible(acc_rate_ci)
    }
  )
}
