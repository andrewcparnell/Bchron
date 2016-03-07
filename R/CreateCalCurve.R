CreateCalCurve = function(name,cal_ages,uncal_ages,one_sigma=rep(0,length(cal_ages))) {

  # This function creates a calibration curve and puts it in the appropriate place for future use with Bchron
  browser()

  # First identify the place where the file needs to go
  file_loc = system.file('data',package='Bchron')

  # Now interpolate so that everything is on a regular grid in calendar years
  cal_order = order(cal_ages,decreasing=TRUE)
  out = as.vector(cbind(cal_ages[cal_order],uncal_ages[cal_order],one_sigma[cal_order]))

  # Now write to a gzipped file
  gz1 = gzfile(paste0(file_loc,'/',name,'.txt.gz'),"w")
  write(cbind(cal_grid,uncal_grid,sigma_grid), gz1)
  close(gz1)

  # And we're done
  cat('Completed!\n')
}