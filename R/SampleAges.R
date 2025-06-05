#' Get sample ages from a set of Bchron calibrated dates
#'
#' A function for extracting sample ages from Bchron calibrated dates
#'
#' @param CalDates A list created from either \code{\link{BchronCalibrate}}.
#' @param n_samp The desired number of samples
#'
#' @details Sometimes it is useful to have a set of sample calendar ages for your calibrated dates. For example the samples might be required to create a credible/confidence interval, or to create another non-trivial function of calibrated dates, such as differences. By default the \code{\link{BchronCalibrate}} function provides a grid of ages and an associated density, similar to OxCal. This function extracts that information and uses the \code{\link{sample}} function to output the desired number of samples
#'
#' @seealso \code{\link{BchronCalibrate}}
#'
#' @return A vector of length \code{n_samp} containing sample ages for the specified date
#' @export
#'
#' @examples
#' # Calibrate multiple ages and summarise them
#' ages <- BchronCalibrate(
#'   ages = c(3445, 11553, 7456), ageSds = c(50, 230, 110),
#'   calCurves = c("intcal20", "intcal20", "shcal20")
#' )
#' # Get samples
#' age_samples <- sampleAges(ages)
#' # Create a credible interval and the median for each date
#' apply(age_samples, 2, quantile, probs = c(0.05, 0.5, 0.95))
sampleAges <- function(CalDates, n_samp = 10000) {
  # Get a set of samples from the current set of dates
  x <- CalDates
  if (is(x, "BchronCalibratedDates")) stop("Object must be created from BchronCalibrate")
  n_dates <- length(x)
  out <- matrix(NA, ncol = n_dates, nrow = n_samp)
  colnames(out) <- names(x)

  for (i in 1:length(x)) {
    currdate <- list(x = x[[i]]$ageGrid, y = x[[i]]$densities)
    out[, i] <- with(
      currdate,
      sample(x,
        size = n_samp,
        replace = TRUE,
        prob = y
      )
    )
  }
  return(out)
}
