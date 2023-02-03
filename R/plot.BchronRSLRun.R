#' Plot output from BchronRSL
#'
#' Plot output from the \code{\link{BchronRSL}} function
#'
#' @param x An object created by \code{\link{BchronRSL}}
#' @param type One of \code{RSL}, \code{rate}, or \code{accel}. If \code{RSL} produces a plot of RSL estimates from the model. If \code{rate}, produces rate estimates. If \code{accel} produces acceleration estimates.
#' @param alpha confidence level used for plotting ellipses
#' @param ellipseCol The colour of the ellipse used for plotting dates
#' @param lineCol The colour of the sea level curve lines
#' @param ... Other arguments to plot (currently ignored)
#'
#' @seealso \code{\link{BchronCalibrate}}, \code{\link{Bchronology}}, \code{\link{BchronRSL}}, \code{\link{BchronDensity}}, \code{\link{BchronDensityFast}}
#'
#' @import ggplot2
#' @importFrom ggforce geom_ellipse
#' @importFrom stats predict qnorm sd
#'
#' @export
plot.BchronRSLRun <-
  function(x,
           type = c("RSL", "rate", "accel"),
           alpha = 0.95,
           ellipseCol = "darkslategray",
           lineCol = "deepskyblue4",
           ...) {

    # Match type
    type <- match.arg(type, several.ok = TRUE)

    # Summarise the ages for later use by everything
    mult <- stats::qnorm(alpha + (1 - alpha) / 2)
    fun1 <- function(x) c(mean(x), mult * stats::sd(x))
    ageAll <- t(apply(
      x$BchronologyRun$thetaPredict,
      2, fun1
    )) %>%
      as.data.frame()

    # Plot raw RSL
    if ("RSL" %in% type) {

      # First create a data frame which has elements
      # age_mean, age_sd
      # position, position_sd
      rslDf <- data.frame(
        Age = ageAll[, 1],
        ageErr = ageAll[, 2],
        RSL = x$RSLmean,
        rslErr = mult * x$RSLsd
      )
      p <- ggplot(data = rslDf, aes(x = Age, y = RSL)) +
        geom_ellipse(aes(
          x0 = Age, y0 = RSL,
          a = ageErr, b = rslErr, angle = 0
        ),
        colour = ellipseCol
        ) +
        theme_bw() +
        scale_x_reverse()

      # Now create predictions
      xgrid <- seq(max(ageAll), min(ageAll), length = 100) / 1000

      predLines <- matrix(NA, ncol = length(xgrid), nrow = nrow(x$samples))
      degmat <- matrix(rep(0:(x$degree), each = length(xgrid)),
        nrow = length(xgrid), ncol = x$degree + 1
      )
      X.pred <- matrix(rep(xgrid - x$const, x$degree + 1), ncol = x$degree + 1)
      X.pred <- (X.pred^degmat)

      for (i in 1:nrow(predLines)) {
        predLines[i, ] <- X.pred %*% matrix(x$samples[i, ], ncol = 1, nrow = x$degree + 1)
      }

      predAll <- data.frame(
        Age = xgrid * 1000,
        RSL = apply(predLines, 2, "quantile", probs = 0.5),
        predLow = apply(predLines, 2, "quantile", probs = (1 - alpha) / 2),
        predHigh = apply(predLines, 2, "quantile", probs = alpha + (1 - alpha) / 2)
      )

      p <- p + geom_line(data = predAll, colour = lineCol) +
        geom_line(
          data = predAll, aes(x = Age, y = predLow),
          linetype = 2, colour = lineCol
        ) +
        geom_line(
          data = predAll, aes(x = Age, y = predHigh),
          linetype = 2, colour = lineCol
        )
    }

    # Plot RSL rate
    if ("rate" %in% type) {

      # No need for ellipses, just create the plot
      xgrid <- seq(max(ageAll), min(ageAll), length = 100) / 1000

      degmat <- matrix(rep(0:(x$degree), each = length(xgrid)),
        nrow = length(xgrid), ncol = x$degree + 1
      )
      degmat_rate <- matrix(rep(0:(x$degree), each = length(xgrid)),
        nrow = length(xgrid), ncol = x$degree + 1
      ) - 1
      X.pred <- matrix(rep(xgrid - x$const, x$degree + 1), ncol = x$degree + 1)
      X.pred <- -degmat * (X.pred^degmat_rate)
      predLines <- matrix(NA, ncol = length(xgrid), nrow = nrow(x$samples))

      for (i in 1:nrow(predLines)) {
        predLines[i, ] <- X.pred %*% matrix(x$samples[i, ], ncol = 1, nrow = x$degree + 1)
      }

      predAll <- data.frame(
        Age = xgrid * 1000,
        Rate = apply(predLines, 2, "quantile", probs = 0.5),
        rateLow = apply(predLines, 2, "quantile", probs = (1 - alpha) / 2),
        rateHigh = apply(predLines, 2, "quantile", probs = alpha + (1 - alpha) / 2)
      )

      p <- ggplot(predAll, aes(x = Age, y = Rate)) +
        geom_line(colour = lineCol) +
        theme_bw() +
        geom_line(aes(x = Age, y = rateLow),
          linetype = 2, colour = lineCol
        ) +
        geom_line(aes(x = Age, y = rateHigh),
          linetype = 2, colour = lineCol
        ) +
        scale_x_reverse()
    }

    # Plot RSL acceleration
    if ("accel" %in% type) {

      # No need for ellipses, just create the plot
      xgrid <- seq(max(ageAll), min(ageAll), length = 100) / 1000

      predLines <- matrix(NA, ncol = length(xgrid), nrow = nrow(x$samples))

      degmat <- matrix(rep(0:(x$degree), each = length(xgrid)),
        nrow = length(xgrid), ncol = x$degree + 1
      )
      degmat_rate <- matrix(rep(0:(x$degree), each = length(xgrid)),
        nrow = length(xgrid), ncol = x$degree + 1
      ) - 1
      degmat <- matrix(rep(0:(x$degree), each = length(xgrid)),
        nrow = length(xgrid), ncol = x$degree + 1
      )
      degmat_accel <- matrix(rep(0:(x$degree), each = length(xgrid)),
        nrow = length(xgrid), ncol = x$degree + 1
      ) - 2
      X.pred <- matrix(rep(xgrid - x$const, x$degree + 1), ncol = x$degree + 1)
      X.pred <- degmat * (degmat - 1) * (X.pred^degmat_accel)

      for (i in 1:nrow(predLines)) {
        predLines[i, ] <- X.pred %*% matrix(x$samples[i, ], ncol = 1, nrow = x$degree + 1)
      }

      predAll <- data.frame(
        Age = xgrid * 1000,
        Acceleration = apply(predLines, 2, "quantile", probs = 0.5),
        accelLow = apply(predLines, 2, "quantile", probs = (1 - alpha) / 2),
        accelHigh = apply(predLines, 2, "quantile", probs = alpha + (1 - alpha) / 2)
      )

      p <- ggplot(predAll, aes(x = Age, y = Acceleration)) +
        geom_line(colour = lineCol) +
        theme_bw() +
        geom_line(aes(x = Age, y = accelLow),
          linetype = 2, colour = lineCol
        ) +
        geom_line(aes(x = Age, y = accelHigh),
          linetype = 2, colour = lineCol
        ) +
        scale_x_reverse()
    }
    p
  }
