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
  
  