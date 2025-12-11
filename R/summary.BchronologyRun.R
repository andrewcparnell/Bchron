#' Summarise a Bchronology object
#'
#' Summarise a \code{\link{Bchronology}} object
#'
#' @param object Output from a run of \code{\link{Bchronology}}
#' @param type Type of output required. The default (quantiles) gives the quantiles of the ages for each position in \code{predictPositions} from \code{\link{Bchronology}}. The other options provide outlier probabilities, convergence diagnostics, accumulation rates, sedimentation rate, and positions of maximum age variance
#' @param probs Probabilities (between 0 and 1) at which to summarise the predicted chronologies
#' @param acc_probs The age range over which to calculate accumulation rates (the default is between the 5\% and 95\% quantiles of all predicted ages)
#' @param useExisting Whether to use the predicted chronologies/positions to calculate the sedimentation rate (if TRUE - default) or to re-create them based on a unit-scaled position grid (if FALSE). The latter will be a little bit slower but will provide better sedimentation rate estimates if the original positions are not on a unit scale (e.g. each cm)
#' @param numPos The number of positions at which to provide the maximum variance
#' @param ... Other arguments (not currently supported)
#' @param digits Number of digits to report values
#' @export
#' 
#' @return A data frame summarising the Bchronology object depending on the type of summary requested. For quantiles, a data frame with the quantiles for each position. For outliers, a data frame with outlier probabilities. For convergence, a data frame with p-values for convergence diagnostics. For sedimentation and accumulation rates, a data frame with the rates at each position/age. For maximum variance, a vector of positions with maximum age variance.
#'
#' @seealso \code{\link{BchronCalibrate}}, \code{\link{Bchronology}} \code{\link{BchronRSL}}, \code{\link{BchronDensity}}, \code{\link{BchronDensityFast}}
#'
summary.BchronologyRun <-
  function(object,
           type = c(
             "quantiles",
             "outliers",
             "convergence",
             "sed_rate",
             "acc_rate",
             "max_var"
           ),
           probs = c(0.025, 0.25, 0.5, 0.75, 0.975),
           acc_probs = c(0.05, 0.95),
           useExisting = TRUE,
           numPos = 3,
           ...,
           digits = max(3, getOption("digits") - 3)) {
    type <- match.arg(type)

    switch(type,
      # Give a list of quantiles for each depth and Show outlier probabilities
      quantiles = {
        chronSummary <- data.frame(cbind(object$predictPositions, round(t(apply(object$thetaPredict, 2, stats::quantile, probs)), 3)))
        colnames(chronSummary) <- c("Depth", paste(probs * 100, "%", sep = ""))
        cat("Quantiles of predicted ages by depth: \n")
        print(round(chronSummary, digits = digits), row.names = FALSE)
        invisible(chronSummary)
      },
      outliers = {
        cat("Posterior outlier probability by date: \n")
        outprob <- data.frame(names(object$calAges), colMeans(object$phi))
        colnames(outprob) <- c("Date", "OutlierProb")
        print(outprob, row.names = FALSE)
        invisible(outprob)
      },
      convergence = {
        cat("Convergence check (watch for too many small p-values): \n")
        pars <- cbind(object$theta, object$phi, object$mu, object$psi)
        n <- ncol(object$theta)
        colnames(pars) <- c(names(object$calAges), paste("Outlier", 1:n), "RateMean", "RateVar")
        geweke <- coda::geweke.diag(pars)[[1]]
        geweke[is.nan(geweke)] <- 0
        pvals <- data.frame(sort(round(c(stats::pnorm(geweke[geweke < 0]), 1 - stats::pnorm(geweke[geweke > 0])), 5)))
        colnames(pvals) <- "p-value"
        print(pvals)
        invisible(pvals)
      },
      sed_rate = {
        cat("\nSedimentation rate (time units per position unit): \n")
        chrons <- object$thetaPredict
        position_grid <- object$predictPositions
        if (useExisting) cat("Note: assumes existing predictPositions are on a regular unit grid. If this is not the case set useExisting = FALSE \n")
        diff_position_grid <- diff(position_grid)
        if (!isTRUE(all.equal(diff_position_grid, rep(1, length(diff_position_grid))))) if (useExisting) warning("predictPositions does not seem to be unit spaced. If not done alreday, set useExisting = FALSE")
        if (!useExisting) {
          position_grid <- seq(min(object$predictPositions), max(object$predictPositions), by = 1)
          chrons <- predict.BchronologyRun(object, newPositions = position_grid)
        }
        sed_rate <- apply(chrons, 1, "diff")
        sed_rate_ci <- cbind(position_grid[-1], t(apply(sed_rate, 1, stats::quantile, probs = probs)))
        colnames(sed_rate_ci)[1] <- "position_grid"
        print(round(sed_rate_ci, digits = digits), row.names = FALSE)
        invisible(as.data.frame(sed_rate_ci))
      },
      acc_rate = {
        cat("\nAccumulation rate (position units per time unit): \n")
        chrons <- object$thetaPredict
        age_grid <- seq(stats::quantile(chrons, acc_probs[1]), stats::quantile(chrons, acc_probs[2]), by = 1)
        my_fun <- function(x) stats::approx(x, y = object$predictPositions, xout = age_grid, rule = 2)$y
        position_interp <- t(apply(chrons, 1, my_fun))
        acc_rate <- apply(position_interp, 1, "diff")
        acc_rate_ci <- cbind(age_grid[-1], t(apply(acc_rate, 1, stats::quantile, probs = probs)))
        colnames(acc_rate_ci)[1] <- "age_grid"
        print(round(acc_rate_ci, digits = digits), row.names = FALSE)
        invisible(as.data.frame(acc_rate_ci))
      },
      max_var = {
        if (numPos == 1) {
          cat("Position with maximum age variance is: \n")
        } else {
          cat("Top", numPos, "positions with maximum age variance are: \n")
        }
        vars <- apply(object$thetaPredict, 2, stats::var)
        o <- order(vars, decreasing = TRUE)
        pos <- object$predictPositions[o[1:numPos]]
        cat(pos, "\n", sep = " ")
        invisible(pos)
      }
    )
  }
