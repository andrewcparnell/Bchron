# Bchron 4.7.4.9000

  - Added in a BchronCheck function to allow for checking of all argument types using checkmate
  - Added in positionEps argument to Bchronology to avoid positions being simulated too close to each other and leading to numerical underflow errors
  - Changed default colours of dates in plot.CalibratedDates and plot.BchronologyRun
  - Added an extra check that all ageSds are bigger than zero
  - Allowed the option for calibrating dates out of calibration curve range
  - Included vdiffr tests for plots
  - Fixed bug that occurred with large 14C values close to upper range of calibration curve
  - Changed the means by which starting values are created. They are now a regular sequence from the top date to the bottom date. This was to fix a bug (issue #17 on GitHub) where a middle extreme date (e.g. non-14C) could cause the starting values of the stratigraphically ordered dates to fall beyond the range of the calibration curve. There's a more general issue here that the starting values are a bit too regular for some cores but no other cores I've tested so far have been broken by this assumption

# Bchron 4.7.4

  - Updated the BchronDensity and BchronDensityFast functions to show lines on top of dates
  - Included ability in above functions to change amount of date transparency
  - Used styler to make the syntax follow tidyverse style guidelines
  - Fixed bug in starting values which occasionally produced values before the extractDate
  - Fixed bug in choosePositions where position chosen was at the edge of the domain

# Bchron 4.7.3

  - Fixed a weird bug where some hdrs were coming out the wrong way in plot.Bchronology
  - Add the ability to create OxCal style plots with the calibration curve
  - Added a feature to plot individual dates in plot.BchronCalibratedDates

# Bchron 4.7.2

  - Included intcal20, shcal20, and marine20 new calibration curves

# Bchron 4.7.1

  - Fixed a bug in Bchronology that caused incorrect predictions

# Bchron 4.7.0

  - Fixed a(nother) small bug in predict.Bchronologies
  - Included proper testthat, travis, and code coverage

# Bchron 4.6.1

  - Fixed bug in predict.Bchronologies

# Bchron 4.6 

Fixed two important bugs:

  - Fixed a mistake in BchronCalibrate that will cause calibrated to have incorrect error ranges compared to OxCal
  - Fixed a problem that occurred in very straight chronologies where a certain parameter would go to zero and cause numerical underflow

# Bchron 4.5

  - Fixed some minor issues with ggplots
  - Added in code for scaling the x-axis

# Bchron 4.4

  - Changed plots to use ggplot2 via ggridges

# Bchron 4.3

Added some new functions:

  - dateInfluence, which works out how influential a single date is in a core
  - coreInfluence, which tries to estimate the total uncertainty reduction/increase between two Bchronology runs
  - choosePositions, which suggest new positions/depths to date to maximally reduce uncertainty
  
  