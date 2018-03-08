#' Find the influence of multiple dates in a pair of Bchronology runs
#' 
#' This function takes as input two \code{\link{Bchronology}} runs and compares the uncertainty intervals. It does this by either: computing the mean uncertainty across the core (\code{type = 'mean'}) at a specified percentile level (e.g. 95%); or by plotting the distribution of uncertainty over the depths in both cores (\code{type = 'distribution'}). Both cores must have the same set of depths at regular depths intervals. 
#'
#' @param bchrRun1 The output of a run of the \code{\link{Bchronology}} function
#' @param bchrRun2 The output of another run of the \code{\link{Bchronology}} function, possibly with different dates. Note this must have the same value of \code{predictPositions} as \code{bchrRun1}
#' @param percentile The value of the percentile to compare the uncertainties. Default is 50%
#' @param type Either \code{'mean'} for the mean level ofuncertainty across the core  of \code{'distribution'} for the distribution of changes
#' @param plot if \code{TRUE} will create a plot of the histograms when using 

#'
#' @details The argument \code{type = 'distribution'} is preferred as it takes account of the full probability distribution of the uncertainty across the core. 
#'
#' @seealso \code{\link{Bchronology}}, \code{\link{dateInfluence.BchronologyRun}} for finding the influence of removing a single date from a core
#'
#' @return Outputs some text providing the influence values for the cores in question. 
#' @export
#'
#' @examples
#' \dontrun{
#' data(Glendalough)
#' GlenOut1 = Bchronology(ages=Glendalough$ages,
#'                        ageSds=Glendalough$ageSds, 
#'                        calCurves=Glendalough$calCurves,
#'                        positions=Glendalough$position, 
#'                        positionThicknesses=Glendalough$thickness,
#'                        ids=Glendalough$id, 
#'                        predictPositions=seq(0,1500,by=10))
#' # Now compare with a run that removes two dates
#' GlenOut2 = Bchronology(ages=Glendalough$ages[-c(3:4)],
#'                        ageSds=Glendalough$ageSds[-c(3:4)], 
#'                        calCurves=Glendalough$calCurves[-c(3:4)],
#'                        positions=Glendalough$position[-c(3:4)], 
#'                        positionThicknesses=Glendalough$thickness[-c(3:4)],
#'                        ids=Glendalough$id[-c(3:4)], 
#'                        predictPositions=seq(0,1500,by=10))
#' 
#' 
#' multiDateInfluence(GlenOut1, GlenOut2, type = 'distribution')
#' }
multiDateInfluence = function(bchrRun1,
                              bchrRun2,
                              type = 'distribution',
                              percentile = 0.95,
                              plot = TRUE) {
  UseMethod('multiDateInfluence')
}

#' @export
multiDateInfluence.BchronologyRun = function(bchrRun1,
                                             bchrRun2,
                                             type = 'distribution',
                                             percentile = 0.95,
                                             plot = TRUE) {
  

  # First check that the two sets of positions match
  positions1 = bchrRun1$predictPositions
  positions2 = bchrRun1$predictPositions
  
  # Create the positions to be used
  usePositions = intersect(positions1,positions2)
  if(length(usePositions) == 0) stop("No overlapping positions between Bchronology runs provided.")
  if(!setequal(usePositions,positions1) | !setequal(usePositions,positions2)) {
    warning(cat('Positions of two Bchron objects not identical. Using:\n', usePositions))
  } 
  
  # Check for non-uniform positions
  if(diff(range(diff(usePositions))) > .Machine$double.eps ^ 0.5) stop("Non-uniform positions used. Please make sure predictPositions are evenly spaced")
  
  # Now summarise the two chronologies
  perc_range = c((1 - percentile)/2, percentile + (1 - percentile)/2)
  summ_1 = t(apply(bchrRun1$thetaPredict, 2, quantile, probs = perc_range))
  summ_2 = t(apply(bchrRun2$thetaPredict, 2, quantile, probs = perc_range))
  
  # Get the difference
  summ_1_diff = apply(summ_1,1,diff)
  summ_2_diff = apply(summ_2,1,diff)
  
  # Extract the bits that matter
  match_dates1 = match(usePositions, positions1)
  match_dates2 = match(usePositions, positions1)
    
  # Final diffs 
  final_diffs = data.frame(positions = usePositions,
                           diff_chron1 = summ_1_diff[match_dates1],
                           diff_chron2 = summ_2_diff[match_dates2])

  # If type is the mean then just report the means and their differences
  cat(paste0('Mean width of chronology 1 at ',100*percentile,'% is ', round(mean(final_diffs$diff_chron1)),' years \n'))
  cat(paste0('Mean width of chronology 2 at ',100*percentile,'% is ', round(mean(final_diffs$diff_chron2)),' years \n'))
  
  # Distribution of widths 
  cat(paste0('Distribution of widths at ', 100*percentile, '% is:\n'))
  cat('Chronology 1\n')
  print(round(quantile(final_diffs$diff_chron1, 
        probs = c(0.025, 0.25, 0.5, 0.75, 0.95, 0.975))))
  cat('Chronology 2\n')
  print(round(quantile(final_diffs$diff_chron2,
        probs = c(0.025, 0.25, 0.5, 0.75, 0.95, 0.975))))
  
  # Difference in widths
  cat(paste0('Distribution of difference in widths at ',100*percentile,'% is:\n'))
  print(round(quantile(final_diffs$diff_chron1 - final_diffs$diff_chron2, 
                 probs = c(0.025, 0.25, 0.5, 0.75, 0.95, 0.975))))
  
  # Histogram
  diff = final_diffs$diff_chron1 - final_diffs$diff_chron2
  hist(diff, breaks = 30,
       xlab = 'Years',
       main = 'Histogram of age difference between Bchron cores',
       freq = FALSE,
       las = 1)
  
  # Probability less than 0
  cat('P(reduction in uncertainty between chornology 1 and chronology 2) = ',round(sum(diff<0)/length(diff),3))
  
}