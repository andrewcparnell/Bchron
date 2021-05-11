#' Check data for input into BchronCalibrate or Bchronology
#'
#' Function to be used for checking the data formats in \code{\link{BchronCalibrate}} and \code{\link{Bchronology}}. Mostly to be used internally to avoid Bchron running into problems with bad data specifications, but might also be useful for
#'
#' @inheritParams BchronCalibrate
#' @inheritParams Bchronology
#' @param type Whether this function has been called to check parameters for calibration purposes (\code{BchronCalibrate}) or chronology purposes (\code{Bchronology})
#'
#' @return This function returns nothing other than a message.
#' @export
#'
#' @import checkmate
#' @examples
#' data(Glendalough)
#'
#' # Check the Glendalough data are in the right format
#' with(
#'   Glendalough,
#'   BchronCheck(ages,
#'     ageSds,
#'     position,
#'     pathToCalCurves = system.file("data", package = "Bchron"),
#'     calCurves,
#'     type = "BchronCalibrate"
#'   )
#' )
BchronCheck <- function(ages,
                        ageSds,
                        positions = NULL,
                        pathToCalCurves = NULL,
                        calCurves = NULL,
                        positionThicknesses = NULL, # Bchronology only down to the end
                        ids = NULL,
                        outlierProbs = NULL,
                        predictPositions = NULL,
                        jitterPositions = NULL,
                        allowOutside = NULL,
                        iterations = NULL,
                        thetaStart = NULL,
                        burn = NULL,
                        thin = NULL,
                        extractDate = NULL,
                        maxExtrap = NULL,
                        thetaMhSd = NULL,
                        muMhSd = NULL,
                        psiMhSd = NULL,
                        ageScaleVal = NULL,
                        positionEps = NULL,
                        positionNormalise = NULL,
                        eps = NULL, # BchronCalibrate only
                        dfs = NULL, # BchronCalibrate only
                        type = c("BchronCalibrate", "Bchronology")) {

  # First check that all three necessary objects have the same length
  assertNumeric(ages, any.missing = FALSE, finite = TRUE)
  nObs <- length(ages)
  assertNumeric(ageSds,
    any.missing = FALSE, len = nObs,
    lower = 0
  )

  # Check that ages and ageSds are whole numbers (i.e. years)
  if (!all(as.integer(ages) == ages)) {
    message("ages not given as whole numbers - rounding occurred")
  }
  if (!all(as.integer(ageSds) == ageSds)) {
    # Smallest sd is 1
    message("ageSds not given as whole numbers - rounding occurred")
  }

  # path to calCurves should be a valid file path
  assertDirectoryExists(pathToCalCurves)
  # Also do some checks here to see if the calibration curves actually exist
  allCalCurves <- unique(calCurves)
  for (i in 1:length(allCalCurves)) {
    calCurveFile <- paste(pathToCalCurves, "/", allCalCurves[i], ".rda", sep = "")
    assertFileExists(calCurveFile)
  }

  if (type == "Bchronology") {
    # Checks for a full chronology run

    # Now do for some of the ones which might be NULL
    assertNumeric(positions, any.missing = FALSE, len = nObs, null.ok = TRUE)
    assertNumeric(positionThicknesses, any.missing = FALSE, len = nObs, null.ok = TRUE)
    assertVector(calCurves, any.missing = FALSE, len = nObs, null.ok = TRUE)
    assertVector(ids, any.missing = FALSE, len = nObs, null.ok = TRUE)
    assertNumeric(outlierProbs, any.missing = FALSE, len = nObs, null.ok = TRUE, lower = 0, upper = 1)
    assertNumeric(thetaStart, any.missing = FALSE, len = nObs, null.ok = TRUE)

    # predictPositions can be any length
    assertNumeric(predictPositions, any.missing = FALSE, null.ok = TRUE)

    # Now some of the other arguments
    assertLogical(positionNormalise, null.ok = TRUE)
    assertLogical(jitterPositions, null.ok = TRUE)
    assertLogical(allowOutside, null.ok = TRUE)
    assertNumber(iterations, lower = 1, null.ok = TRUE)
    assertNumber(burn, lower = 1, null.ok = TRUE)
    assertNumber(thin, lower = 1, null.ok = TRUE)
    assertNumber(extractDate, finite = TRUE, null.ok = TRUE)
    assertNumber(maxExtrap, finite = TRUE, null.ok = TRUE)
    assertNumber(thetaMhSd, finite = TRUE, null.ok = TRUE)
    assertNumber(muMhSd, finite = TRUE, null.ok = TRUE)
    assertNumber(psiMhSd, finite = TRUE, null.ok = TRUE)
    assertNumber(ageScaleVal, finite = TRUE, null.ok = TRUE)
    assertNumber(positionEps, finite = TRUE, null.ok = TRUE)
  } else {

    # Do some of the checks for BchronCalibrate
    assertNumber(eps, finite = TRUE, null.ok = TRUE)
    assertNumeric(dfs, finite = TRUE, null.ok = TRUE, len = nObs)
  }
}
