#' Find the influence of dates in a pair of Bchronology runs across the core
#'
#' This function takes as input two \code{\link{Bchronology}} runs and compares the uncertainty intervals. It does this by
#' computing the mean uncertainty across the core (\code{type = 'mean'}) at a specified percentile level (e.g. 95\%) and
#' subsequently reporting the reduction/increase in uncertainty between the two runs. Both cores must
#' have the same set of depths/positions at regular intervals.
#'
#' @param bchrRun1 The output of a run of the \code{\link{Bchronology}} function
#' @param bchrRun2 The output of another run of the \code{\link{Bchronology}} function, possibly with different dates.
#' Note this must have the same value of \code{predictPositions} as \code{bchrRun1}
#' @param percentile The value of the percentile to compare the uncertainties. Default is 95\%
#' @param type if \code{plot} will return a plot of the difference in uncertainties at the specified percentile level.
#' If \code{summary} will return text output of the reduction in uncertainty at each position. If \code{max} will return the
#' position of the maximum decrease in uncertainty and a list of all the positions where the reduction in uncertainty exceeds the value of
#' \code{ageTolerance}
#' @param ageTolerance A value in years for which to report the positions at which the reduction in uncertainty exceeds this value.
#' @param ... Additional arguments to plot
#'
#' @details For example, if the \code{ageTolerance} value is 500 years, then \code{coreInfluence} will return all of the positions at
#' which the uncertainty reduction is bigger than 500.
#'
#' @seealso \code{\link{Bchronology}},  \code{\link{choosePositions}}, \code{\link{dateInfluence}} for finding the influence of removing a single date from a core
#'
#' @return Depending on type will outputs some text and plots providing the influence values for the cores in question.
#' @export
#'
#' @examples
#' \donttest{
#' data(Glendalough)
#' # Start with a run that remove two dates
#' GlenOut1 <- Bchronology(
#'   ages = Glendalough$ages[-c(3:4)],
#'   ageSds = Glendalough$ageSds[-c(3:4)],
#'   calCurves = Glendalough$calCurves[-c(3:4)],
#'   positions = Glendalough$position[-c(3:4)],
#'   positionThicknesses = Glendalough$thickness[-c(3:4)],
#'   ids = Glendalough$id[-c(3:4)],
#'   predictPositions = seq(0, 1500, by = 10)
#' )
#' GlenOut2 <- Bchronology(
#'   ages = Glendalough$ages,
#'   ageSds = Glendalough$ageSds,
#'   calCurves = Glendalough$calCurves,
#'   positions = Glendalough$position,
#'   positionThicknesses = Glendalough$thickness,
#'   ids = Glendalough$id,
#'   predictPositions = seq(0, 1500, by = 10)
#' )
#'
#' # Now compare their influence
#' coreInfluence(GlenOut1,
#'   GlenOut2,
#'   type = c("max", "plot"),
#'   xlab = "Age (cal years BP)",
#'   ylab = "Depth (cm)",
#'   main = "Chronology difference at 95% for
#'               Glendalough removing two dates",
#'   las = 1
#' )
#' }
coreInfluence <- function(bchrRun1,
                          bchrRun2,
                          percentile = 0.95,
                          type = c("plot", "summary", "max"),
                          ageTolerance = 500,
                          ...) {
  UseMethod("coreInfluence")
}

#' @export
coreInfluence.BchronologyRun <- function(bchrRun1,
                                         bchrRun2,
                                         percentile = 0.95,
                                         type = c("plot", "summary", "max"),
                                         ageTolerance = 500,
                                         ...) {
  # Get the type
  type <- match.arg(type, several.ok = TRUE)

  # First check that the two sets of positions match
  positions1 <- bchrRun1$predictPositions
  positions2 <- bchrRun1$predictPositions

  # Create the positions to be used
  usePositions <- intersect(positions1, positions2)
  if (length(usePositions) == 0) stop("No overlapping positions between Bchronology runs provided.")
  if (!setequal(usePositions, positions1) | !setequal(usePositions, positions2)) {
    warning(paste("Positions of two Bchron objects not identical. Using:\n", usePositions))
  }

  # Check for non-uniform positions
  if (diff(range(diff(usePositions))) > .Machine$double.eps^0.5) stop("Non-uniform positions used. Please make sure predictPositions are evenly spaced")

  # Now summarise the two chronologies
  perc_range <- c((1 - percentile) / 2, percentile + (1 - percentile) / 2)
  summ_1 <- t(apply(bchrRun1$thetaPredict, 2, "quantile",
    probs = perc_range
  ))
  summ_2 <- t(apply(bchrRun2$thetaPredict, 2, "quantile",
    probs = perc_range
  ))

  # Get the difference
  summ_1_diff <- apply(summ_1, 1, diff)
  summ_2_diff <- apply(summ_2, 1, diff)

  # Extract the bits that matter
  match_dates1 <- match(usePositions, positions1)
  match_dates2 <- match(usePositions, positions1)

  # Final diffs
  diffs <- data.frame(
    positions = usePositions,
    diff_chron1 = summ_1_diff[match_dates1],
    diff_chron2 = summ_2_diff[match_dates2],
    all_diff = summ_1_diff[match_dates1] - summ_2_diff[match_dates2]
  )

  # Now report depending on type
  if ("plot" %in% type) {
    # Return a plot of the chronology differences
    with(diffs, plot(all_diff, positions,
      # ylim = rev(range(positions)),
      type = "l",
      ...
    ))
    graphics::abline(v = 0)
  }
  if ("summary" %in% type) {
    return(diffs)
  }
  if ("max" %in% type) {
    # Report the position of the maximum uncertainty (type = max) ...
    max_change <- which.max(diffs$all_diff)
    message(paste0("Position of maximum age uncertainty change is: ", diffs$positions[max_change], "\n"))
    message(paste0("with age uncertainty reduction of ", diffs$all_diff[max_change], " years \n\n"))

    # ...and a window of uncertainty around that (in depth)
    positionsAll <- which(diffs$all_diff > ageTolerance)
    df2 <- with(diffs, data.frame(
      positions = positions[positionsAll],
      ages = all_diff[positionsAll]
    ))
    message(paste0("Positions with age uncertainty change above ", ageTolerance, " years are:\n"))
    message(diffs$positions[positionsAll])
    invisible(df2)

    if ("plot" %in% type) {
      with(df2, polygon(c(ages, rep(ageTolerance, length(ages))), c(positions, rev(positions)),
        col = "gray",
        border = NA
      ))
    }
  }
}
