#' Compute positions to date next which result in maximal decrease of chronological uncertainty
#'
#' This function finds, for a given current chronology, created via
#' \code{\link{Bchronology}}, which positions (depths) to date next
#' If N = 1 it just finds the position with the biggest uncertainty
#' If N>1 it puts a date at the N = 1 position and re-runs Bchronology
#' with the extra psuedo date. It uses the \code{\link{unCalibrate}} function
#' with the un-calibrated age estimated at the median of the chronology
#'  and the sd as specified via the \code{newSds} argument. Other arguments 
#'  specify the new thicknesses, calibration curves, and outlier probabilities 
#'  for newly inserted psuedo-dates.
#'
#' @param bchrRun A run of the current chronology as output from \code{\link{Bchronology}}
#' @param N The number of new positions required
#' @param newSds The new standard deviations of the psuedo-added dates
#' @param newThicknesses The new thicknesses of the psuedo-added dates
#' @param positions The positions allowed to estimate the new positions to date. Defaults to the value of \code{predictPositions} from the Bchronology run
#' @param newCalCurve The new calibration curve of the psuedo-added dates
#' @param newOutlierProb  The new outlier probabilities of the psuedo-added dates
#' @param level The confidence level required for minimising the uncertainty. Defaults to 50\%. (Note: this will be estimated more robustly than the 95\% level)
#' @param plot Whether to plot the chronologies as they are produced
#' @param count Counter function (not for use other than by the function itself)
#' @param linesAt Horizontal line positions (not for use other than by the function itself)
#'
#' @return Some plots and the positions to date next
#' @export
#'
#' @seealso \code{\link{Bchronology}} for the main function to create chronologies, \code{\link{unCalibrate}} for the ability to invert calendar dates for a given calibration curve.
#'
#' @examples
#' \donttest{
#' data(Glendalough)
#' GlenOut = Bchronology(ages=Glendalough$ages,
#'                       ageSds=Glendalough$ageSds,
#'                       calCurves=Glendalough$calCurves,
#'                       positions=Glendalough$position,
#'                       positionThicknesses=Glendalough$thickness,
#'                       ids=Glendalough$id,
#'                       predictPositions=seq(0,1500,by=10))
#' 
#' # Find out which two positions (depths) to date if we have room for two more dates
#' # Here going to choose 3 new positions to date
#' newPositions = choosePositions(GlenOut, N = 3)
#' print(newPositions)
#' }
choosePositions = function(bchrRun,
                           N = 1,
                           newSds = 30,
                           newThicknesses = 0,
                           positions = bchrRun$predictPositions,
                           newCalCurve = 'intcal13',
                           newOutlierProb = 0.05,
                           level = 0.5,
                           plot = TRUE,
                           count = 1,
                           linesAt = NULL) {
  UseMethod('choosePositions')
}

#' @export
choosePositions.BchronologyRun = function(bchrRun,
                                    N = 1,
                                    newSds = 30,
                                    newThicknesses = 1,
                                    positions = bchrRun$predictPositions,
                                    newCalCurve = 'intcal13',
                                    newOutlierProb = 0.05,
                                    level = 0.5,
                                    plot = TRUE,
                                    count = 1,
                                    linesAt = NULL) {
  
  # Function to find which positions to date next
  # Calls itself recursively. If N = 1 just find the position with the 
  # biggest uncertainty. 
  # If N>1 it puts a date at the N = 1 position and re-calibrates the model
  # with the 14C age estimated at the median of the chronology and the sd
  # as specified in the function. It then recursively calls itself with N = N-1
  
  if(N == 1 & count == 1) {
    if(isTRUE(all.equal(positions,bchrRun$predictPositions))) {
      cat('Using predict positions from Bchron run provided.\n')
    } else {
      cat('Predicting new positions for object.\n')
      positions = predict.BchronologyRun(bchrRun, 
                          newPositions = positions)
    }
  }
  
  main = ifelse(count == 1, 'Bchronology plot with position of maximum uncertainty', 'Bchronology plot with extra psuedo-dates')
  if(plot) plot(bchrRun, main = main)
  
  # First find the position which the biggest uncertainty
  
  # Find lower and upper confidence levels
  lower = (1 - level)/2
  upper = level + lower
  # Use capture.output to suppress printing of summary
  currUnc = apply(apply(bchrRun$thetaPredict, 
                         2, 'quantile', 
                         probs = c(lower, upper)),
                   2, diff)
  
  # If N is 1 then return the position with the max uncertainty
  returnPos = positions[which.max(currUnc)]
  linesAt = c(linesAt, returnPos)
  if(plot) graphics::abline(h = linesAt)
  store = sprintf("osition with largest uncertainty at %s%% level is %s",
            signif(level*100, 3), signif(returnPos, 3))
  if(N > 1 | count > 1) cat('Round', count, '\n')
  if(count>1) {
    cat('Next p',store, '\n', sep = '') 
  } else {
    cat('P',store, '\n', sep = '')  
  }
  
  if(N > 1) {
    # Find the biggest uncertain date and add it in to the mix
    newCalDate = round(stats::median(bchrRun$thetaPredict[,which.max(currUnc)]))
    # Need to uncalibrate this based on the calibration curve required
    newDate = round(unCalibrate(newCalDate, calCurve = newCalCurve, type = 'ages'))
    newPos = returnPos
    
    # Now run a new Bchronology run
    oldInput = bchrRun$inputVals
    
    # Find where the new position lies in the input positions
    posPlaceBin = findInterval(oldInput$positions, newPos)
    posPlace = min(which(posPlaceBin == 1))
    
    # Need to specify new dates, sds, positions, positionThicknesses, calCurves,
    # outlierprobs, and predictPositions
    blankVec = rep(0, length(oldInput$ages) + 1)
    blankVec[posPlace] = NA
    agesNew = sdNew = positionsNew = positionThicknessesNew = outlierProbsNew = blankVec
    calCurvesNew = unlist(list(oldInput$calCurves, oldInput$calCurves[1]))
    idsNew = unlist(list(oldInput$ids, oldInput$ids[1]))
      
    # Now go through and fill in all the info
    agesNew[!is.na(blankVec)] = oldInput$ages
    agesNew[posPlace] = newDate 
    sdNew[!is.na(blankVec)] = oldInput$ageSds
    sdNew[posPlace] = newSds 
    positionsNew[!is.na(blankVec)] = oldInput$positions
    positionsNew[posPlace] = newPos 
    positionThicknessesNew[!is.na(blankVec)] = oldInput$positionThicknesses
    positionThicknessesNew[posPlace] = newThicknesses 
    calCurvesNew[!is.na(blankVec)] = oldInput$calCurves
    levels(calCurvesNew) = c(levels(calCurvesNew), newCalCurve)
    calCurvesNew[posPlace] = newCalCurve 
    idsNew[!is.na(blankVec)] = oldInput$ids
    levels(idsNew) = c(levels(oldInput$ids), 'newDate')
    idsNew[posPlace] = 'newDate' 
    outlierProbsNew[!is.na(blankVec)] = oldInput$outlierProbs
    outlierProbsNew[posPlace] = newOutlierProb

    cat('\n')
    store = sprintf("Calibrating with new age %s with sd %s at position %s",
                    newDate, newSds, signif(returnPos, 3))
    cat(store, '\n')
    
    
    # Run Bchron on this new data set
    newOut = Bchronology(ages=agesNew,
                         ageSds=sdNew,
                         calCurves=calCurvesNew,
                         positions=positionsNew,
                         positionThicknesses=positionThicknessesNew,
                         ids=idsNew,
                         outlierProbs = outlierProbsNew,
                         predictPositions = positions)
    cat('\n')
    
    if(plot) plot(newOut, main = 'Bchronology plot with extra psuedo-dates')
    
    # Now run the function again with N = N - 1
    linesAt = choosePositions(newOut,
                              N = N - 1,
                              newSds = newSds,
                              newThicknesses = newThicknesses,
                              positions = positions,
                              newCalCurve = newCalCurve,
                              newOutlierProb = newOutlierProb,
                              level = level,
                              count = count + 1,
                              linesAt = linesAt)

  }
  invisible(linesAt)
}