data(Glendalough)
GlenOut = Bchronology(ages=Glendalough$ages,
ageSds=Glendalough$ageSds,
calCurves=Glendalough$calCurves,
positions=Glendalough$position,
positionThicknesses=Glendalough$thickness,
ids=Glendalough$id,
predictPositions=seq(0,1500,by=10))
choosePositions.BchronologyRun(GlenOut, N = 1)

choosePositions = function(bchrRun,
                           N = 1,
                           newSds = 30,
                           positions = bchrRun$predictPositions,
                           calCurve = 'intcal13',
                           level = 0.5) {
  UseMethod('dateInfluence')
}

#' @export
choosePositions.BchronologyRun = function(bchrRun,
                                    N = 1,
                                    newSds = 30,
                                    positions = bchrRun$predictPositions,
                                    calCurve = 'intcal13',
                                    level = 0.5) {
  
  # Function to find which positions to date next
  # Calls itself recursively. If N = 1 just find the position with the 
  # biggest uncertainty. 
  # If N>1 it puts a date at the N = 1 position and re-calibrates the model
  # with the 14C age estimated at the median of the chronology and the sd
  # as specified in the function. It then recursively calls itself with N = N-1
  
  if(isTRUE(all.equal(positions == bchrRun$predictPositions))) {
    cat('Using predict positions from Bchron run provided.\n')
  } else {
    cat('Predicting new positions for object.\n')
    positions = predict(bchrRun, 
                        newPositions = positions)
  }
  
  # First find the position which the biggest uncertainty
  
  # Find lower and upper confidence levels
  lower = (1 - level)/2
  upper = level + lower
  # Use capture.output to suppress printing of summary
  currUnc = apply(apply(bchrRun$thetaPredict, 
                         2, quantile, 
                         probs = c(lower, upper)),
                   2, diff)
  
  # If N is 1 then return the position with the max uncertainty
  returnPos = positions[which.max(currUnc)]
  if(N == 1) {
    sprintf("Position with largest uncertainty at %s%% level is %s",
            signif(level*100, 3), signif(returnPos, 3))
    invisible(returnPos)
  } else {
    # Find the biggest uncertain date and add it in to the mix
    newDate = round(median(bchrRun$thetaPredict[,which.max(currUnc)]))
    newPos = returnPos
    
    # Now run a new Bchronology run
    oldInput = bchrRun$inputVals
    
    # Find where the new position lies in the input positions
    posPlaceBin = findInterval(oldInput$positions, newPos)
    posPlace = min(which(posPlaceBin == 1))
    
    # Need to specify new dates, sds, positions, positionThicknesses, calCurves,
    # outlierprobs, and predictPositions
    blankVec = rep(0, length(oldInput$ages))
    blankVec[posPlace] = NA
    agesNew = sdNew = positionsNew
    agesNew[is.na(blankVec)] = oldInput$ages
    agesNew[posPlace] = newDate 
      
    z2 <- numeric(length(z1)+length(id)) 
    z2[id] <- NA 
    z2[!is.na(z2)] <- z1 
      
      : int [1:6] 0 2310 9150 9810 10940 11550
    $ ageSds             : int [1:6] 1 60 50 60 60 60
    $ positions          : int [1:6] 0 426 1166 1204 1383 1433
    $ positionThicknesses: int [1:6] 0 4 4 4 2 2
    $ calCurves          : Factor w/ 2 levels "intcal13","normal": 2 1 1 1 1 1
    $ ids                : Factor w/ 6 levels "Beta-100897",..: 6 5 4 3 2 1
    $ outlierProbs       : num [1:6] 0.01 0.01 0.01 0.01 0.01 0.01
    $ predictPositions   : num [1:151] 0 10 20 30 40 50 60 70 80 90 ...
    
  }
  
  
}