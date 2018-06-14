#' Find the influence of the dates in a Bchronology run
#' 
#' This function takes as input a \code{\link{Bchronology}} run and allows the user to estimate a value of 'influence' for either a particular date (by name or number), for all dates in a core (\code{whichDate = 'all'}), or for all internal dates (\code{whichDate = 'internal'}). It measures the influence by either the Kullback-Leibler divergence (\code{KL}), the absolute mean difference (\code{absMeanDiff}), or the absolute median difference (\code{absMedianDiff}).
#'
#' @param bchrRun The output of a run of the \code{\link{Bchronology}} function
#' @param whichDate The chosen date to remove. Either \code{'all'} which removes each date in turn, or \code{'internal'} which removes all but the top/bottom dates, or the date number (in the order same order as in argument 1), or the name of the date from the Bchronology run output file.
#' @param measure Either \code{'KL'} for Kullback Leibler divergence (recommended); or \code{'absMeanDiff'} or \code{'absMedianDiff'} for distances in years from the mean/median age respectively
#'
#' @details The \code{KL} measure is preferred as it takes account of the full probability distributions but it lacks a simple interpretation. The best way to use it is with \code{whichDate = 'all'}: the largest value corresponds to the most influential date in the chronology. For simpler interpretation use \code{measure = 'absMeanDiff'} or \code{measure = 'absMedianDiff'} as for these the influence is measured in years. 
#' 
#' When the predictPositions from the original \code{Bchronology} run do not include those of the date(s) being left out then the function uses the closest position and reports a warning.
#'
#' @seealso \code{\link{Bchronology}}, \code{\link{summary.BchronologyRun}}, \code{\link{coreInfluence}}, \code{\link{choosePositions}}
#'
#' @return Outputs some text providing the influence values for the date(s) in question. If given an assignment value also return a list containing all the probabiliy distributions.
#' @export
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
#' dateInfluence(GlenOut, whichDate = 4, measure = 'absMeanDiff')
#' }
dateInfluence = function(bchrRun,
                         whichDate = 'all',
                         measure = c('KL', 
                                     'absMeanDiff', 
                                     'absMedianDiff')) {
  UseMethod('dateInfluence')
}

#' @export
dateInfluence.BchronologyRun = function(bchrRun,
                                        whichDate = 'all',
                                        measure = c('KL', 
                                                    'absMeanDiff', 
                                                    'absMedianDiff')) {
  
  # Function to calculuate influence between two sets of date samples
  measureInfluence = function(s1, s2, measure) {
    switch(measure, 
      KL = {
        # KL divergence defined as sum_i p(i) log(p(i)/q(i)
        
        # Calculate densities
        s1Dens = stats::density(s1, 
                         from = min(c(s1, s2)), 
                         to = max(c(s1, s2)))$y
        s2Dens = stats::density(s2, 
                         from = min(c(s1, s2)), 
                         to = max(c(s1, s2)))$y
        
        # Re-scale them so that they sum to 1
        s1DensResc = s1Dens/sum(s1Dens)
        s2DensResc = s2Dens/sum(s2Dens)
        
        # Calculate the KL divergence
        int = s1DensResc*log(s1DensResc/s2DensResc)
        # Get rid of NA values created above
        int = int[s1DensResc>0]
        int = int[!is.infinite(int)]
        out = sum(int)
      },
      absMeanDiff = {
        out = abs(mean(s1) - mean(s2))
      },
      absMedianDiff = {
        out = abs(stats::median(s1) - stats::median(s2))
      }
    )
    return(out)
  }
  
  if(is.numeric(whichDate)) {
    # If whichDate is a number then leave out that date
    nDates = length(bchrRun$calAges)
    if(!(whichDate %in% (1:nDates))) stop(paste("whichDate must be an integer from 1 to",nDates))
    
    # Find the closest predictPosition to this date
    closestPositionIndex = which.min(abs(bchrRun$inputVals$predictPositions - bchrRun$inputVals$positions[whichDate]))
    closestPosition = bchrRun$inputVals$predictPositions[closestPositionIndex]
    if(abs(closestPosition - bchrRun$inputVals$positions[whichDate]) < .Machine$double.eps) warning(paste("The Bchron run provided does not have position",bchrRun$inputVals$positions[whichDate],"included in the predictPositions argument. Using closest predictPosition ",closestPosition, 'instead.'))
    
    # Leave that date out and re-run
    newRun = with(bchrRun$inputVals,
                  Bchronology(ages = ages[-whichDate],
                              ageSds = ageSds[-whichDate],
                              positions = positions[-whichDate],
                              positionThicknesses = positionThicknesses[-whichDate],
                              calCurves = calCurves[-whichDate],
                              ids = ids[-whichDate],
                              outlierProbs = outlierProbs[-whichDate],
                              predictPositions = closestPosition,
                              pathToCalCurves = pathToCalCurves,
                              jitterPositions = jitterPositions,
                              iterations = iterations,
                              burn = burn,
                              thin = thin,
                              extractDate = extractDate,
                              maxExtrap = maxExtrap,
                              thetaMhSd = thetaMhSd,
                              muMhSd = muMhSd,
                              psiMhSd = psiMhSd,
                              ageScaleVal = ageScaleVal,
                              positionScaleVal = positionScaleVal))
    
    newRunAgePredict = newRun$thetaPredict
    whichPredPos = match(closestPosition,
                         bchrRun$predictPositions)
    oldRunAgePredict = bchrRun$thetaPredict[,whichPredPos]
    
    # Remove missing values if required
    if(any(is.na(newRunAgePredict)) && whichDate ==1) {
      warning("Some missing values in predicted ages, likely as a result of a top date being removed. These missing values will be removed.")
      newRunAgePredict = newRunAgePredict[!is.na(newRunAgePredict)]
    }
    
    # Measure the influence
    influence = measureInfluence(newRunAgePredict,
                               oldRunAgePredict,
                               measure = measure)
    
    # Create output list
    out = list(measure = measure, 
               influence = influence,
               usedPosition = closestPosition,
               desiredPosition = bchrRun$inputVals$positions[whichDate],
               newRunAgePredict = newRunAgePredict,
               oldRunAgePredict = oldRunAgePredict)
    
    # Report
    dateName = bchrRun$inputVals$id[whichDate]
    cat('\nInfluence of date:', as.character(dateName), 'is', influence, 'using measure', measure, '\n')
    
  } else if(whichDate %in% names(bchrRun$calAges)){
    # If whichDate is one of the named dates
    dateNames = bchrRun$inputVals$ids
    if(!(whichDate %in% dateNames)) stop(paste("whichDate not found in date names. Must be one of: ",paste(dateNames, collapse = ' ')))
    
    whichDate = match(whichDate, dateNames)
    # The predictPositions must include this date
    # Find the closest predictPosition to this date
    closestPositionIndex = which.min(abs(bchrRun$inputVals$predictPositions - bchrRun$inputVals$positions[whichDate]))
    closestPosition = bchrRun$inputVals$predictPositions[closestPositionIndex]
    if(abs(closestPosition - bchrRun$inputVals$positions[whichDate]) < .Machine$double.eps) warning(paste("The Bchron run provided does not have position",bchrRun$inputVals$positions[whichDate],"included in the predictPositions argument. Using closest predictPosition ",closestPosition, 'instead.'))
    
    # Leave that date out and re-run
    newRun = with(bchrRun$inputVals,
                  Bchronology(ages = ages[-whichDate],
                              ageSds = ageSds[-whichDate],
                              positions = positions[-whichDate],
                              positionThicknesses = positionThicknesses[-whichDate],
                              calCurves = calCurves[-whichDate],
                              ids = ids[-whichDate],
                              outlierProbs = outlierProbs[-whichDate],
                              predictPositions = positions[whichDate],
                              pathToCalCurves = pathToCalCurves,
                              jitterPositions = jitterPositions,
                              iterations = iterations,
                              burn = burn,
                              thin = thin,
                              extractDate = extractDate,
                              maxExtrap = maxExtrap,
                              thetaMhSd = thetaMhSd,
                              muMhSd = muMhSd,
                              psiMhSd = psiMhSd,
                              ageScaleVal = ageScaleVal,
                              positionScaleVal = positionScaleVal))
    
    newRunAgePredict = newRun$thetaPredict
    whichPredPos = match(closestPosition,
                         bchrRun$predictPositions)
    oldRunAgePredict = bchrRun$thetaPredict[,whichPredPos]
    
    # Remove missing values if required
    if(any(is.na(newRunAgePredict)) && whichDate ==1) {
      warning("Some missing values in predicted ages, likely as a result of a top date being removed. These missing values will be removed.")
      newRunAgePredict = newRunAgePredict[!is.na(newRunAgePredict)]
    }
    
    # Measure the influence
    influence = measureInfluence(newRunAgePredict,
                               oldRunAgePredict,
                               measure = measure)
    
    # Create output list
    out = list(measure = measure, 
               influence = influence,
               usedPosition = closestPosition,
               desiredPosition = bchrRun$inputVals$positions[whichDate],
               newRunAgePredict = newRunAgePredict,
               oldRunAgePredict = oldRunAgePredict)
    
    # Report
    dateName = bchrRun$inputVals$id[whichDate]
    cat('\nInfluence of date:', as.character(dateName), 'is', influence, 'using measure', measure, '\n')
    
  } else if(whichDate == 'all') {
    # If whichDate is all of the dates run full cross validation
    
    nDates = length(bchrRun$calAges)
    dateNames = bchrRun$inputVals$ids
    influence = rep(NA, nDates)
    out = vector('list', length = nDates)
    
    # Start loop through 
    for (i in 1:nDates) {
      cat('Leaving out date',as.character(dateNames[i]), '\n\n')
      whichDate = i

      # Find the closest predictPosition to this date
      closestPositionIndex = which.min(abs(bchrRun$inputVals$predictPositions - bchrRun$inputVals$positions[whichDate]))
      closestPosition = bchrRun$inputVals$predictPositions[closestPositionIndex]
      if(abs(closestPosition - bchrRun$inputVals$positions[whichDate]) < .Machine$double.eps) warning(paste("The Bchron run provided does not have position",bchrRun$inputVals$positions[whichDate],"included in the predictPositions argument. Using closest predictPosition ",closestPosition, 'instead.'))
    
      # Leave that date out and re-run
      newRun = with(bchrRun$inputVals,
                    Bchronology(ages = ages[-whichDate],
                                ageSds = ageSds[-whichDate],
                                positions = positions[-whichDate],
                                positionThicknesses = positionThicknesses[-whichDate],
                                calCurves = calCurves[-whichDate],
                                ids = ids[-whichDate],
                                outlierProbs = outlierProbs[-whichDate],
                                predictPositions = closestPosition,
                                pathToCalCurves = pathToCalCurves,
                                jitterPositions = jitterPositions,
                                iterations = iterations,
                                burn = burn,
                                thin = thin,
                                extractDate = extractDate,
                                maxExtrap = maxExtrap,
                                thetaMhSd = thetaMhSd,
                                muMhSd = muMhSd,
                                psiMhSd = psiMhSd,
                                ageScaleVal = ageScaleVal,
                                positionScaleVal = positionScaleVal))
    
      newRunAgePredict = newRun$thetaPredict
      whichPredPos = match(closestPosition,
                           bchrRun$predictPositions)
      oldRunAgePredict = bchrRun$thetaPredict[,whichPredPos]
      
      # Remove missing values in the case of top ages being removed
      if(any(is.na(newRunAgePredict)) && i ==1) {
        warning("Some missing values in predicted ages, likely as a result of a top date being removed. These missing values will be removed.")
        newRunAgePredict = newRunAgePredict[!is.na(newRunAgePredict)]
      }
      
      # Measure the influence
      influence[i] = measureInfluence(newRunAgePredict,
                                      oldRunAgePredict,
                                      measure = measure)
      
      # Create output list
      out[[i]] = list(dateName = dateNames[i],
                      measure = measure, 
                      influence = influence[i],
                      usedPosition = closestPosition,
                      desiredPosition = bchrRun$inputVals$positions[whichDate],
                      newRunAgePredict = newRunAgePredict,
                      oldRunAgePredict = oldRunAgePredict)
      
    } # End loop through dates

    cat(paste0('\nInfluence of dates using measure ', measure, ':\n'))
    print(data.frame(Name = as.character(dateNames), Influence = influence))
    
  } else if(whichDate == 'internal') {
    # If whichDate is all of the internal dates miss out all but first or last
    
    nDates = length(bchrRun$calAges)
    dateNames = bchrRun$inputVals$ids
    influence = rep(NA, nDates)
    out = vector('list', length = nDates)
    
    # Start loop through 
    for (i in 2:(nDates-1)) {
      cat('Leaving out date',as.character(dateNames[i]), '\n\n')
      whichDate = i
      
      # Find the closest predictPosition to this date
      closestPositionIndex = which.min(abs(bchrRun$inputVals$predictPositions - bchrRun$inputVals$positions[whichDate]))
      closestPosition = bchrRun$inputVals$predictPositions[closestPositionIndex]
      if(abs(closestPosition - bchrRun$inputVals$positions[whichDate]) < .Machine$double.eps) warning(paste("The Bchron run provided does not have position",bchrRun$inputVals$positions[whichDate],"included in the predictPositions argument. Using closest predictPosition ",closestPosition, 'instead.'))
      
      # Leave that date out and re-run
      newRun = with(bchrRun$inputVals,
                    Bchronology(ages = ages[-whichDate],
                                ageSds = ageSds[-whichDate],
                                positions = positions[-whichDate],
                                positionThicknesses = positionThicknesses[-whichDate],
                                calCurves = calCurves[-whichDate],
                                ids = ids[-whichDate],
                                outlierProbs = outlierProbs[-whichDate],
                                predictPositions = closestPosition,
                                pathToCalCurves = pathToCalCurves,
                                jitterPositions = jitterPositions,
                                iterations = iterations,
                                burn = burn,
                                thin = thin,
                                extractDate = extractDate,
                                maxExtrap = maxExtrap,
                                thetaMhSd = thetaMhSd,
                                muMhSd = muMhSd,
                                psiMhSd = psiMhSd,
                                ageScaleVal = ageScaleVal,
                                positionScaleVal = positionScaleVal))
      
      newRunAgePredict = newRun$thetaPredict
      whichPredPos = match(closestPosition,
                           bchrRun$predictPositions)
      oldRunAgePredict = bchrRun$thetaPredict[,whichPredPos]
      
      # Remove missing values in the case of top ages being removed
      if(any(is.na(newRunAgePredict)) && i ==1) {
        warning("Some missing values in predicted ages, likely as a result of a top date being removed. These missing values will be removed.")
        newRunAgePredict = newRunAgePredict[!is.na(newRunAgePredict)]
      }
      
      # Measure the influence
      influence[i] = measureInfluence(newRunAgePredict,
                                      oldRunAgePredict,
                                      measure = measure)
      
      # Create output list
      out[[i]] = list(dateName = dateNames[i],
                      measure = measure, 
                      influence = influence[i],
                      usedPosition = closestPosition,
                      desiredPosition = bchrRun$inputVals$positions[whichDate],
                      newRunAgePredict = newRunAgePredict,
                      oldRunAgePredict = oldRunAgePredict)
      
    } # End loop through dates
    
    cat(paste0('\nInfluence of dates using measure ', measure, ':\n'))
    print(data.frame(Name = as.character(dateNames), Influence = influence))
    
  } else {
    stop("whichDate must be either 'all', or an integer, or the name of the date")
  }
  
  invisible(out)
}