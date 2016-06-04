SampleAges = function(CalDates, n_samp = 1000) {
  # Get a set of samples from the current set of dates
  x = CalDates
  if(class(x) != 'BchronCalibratedDates') stop('Object must be creaed from BchronCalibrate or Bchronology')
  n_dates = length(x)
  out = matrix(NA, ncol = n_dates, nrow = n_samp) 
  colnames(out) = names(x)
  
  for(i in 1:length(x)) {
    currdate = list(x=x[[i]]$ageGrid,y=x[[i]]$densities)
    out[,i] = with(currdate, 
                   sample(x, 
                          size = n_samp, 
                          replace = TRUE, 
                          prob = y))
  }
  return(out)
}