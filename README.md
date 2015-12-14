[![cran version](http://www.r-pkg.org/badges/version/Bchron)](http://cran.rstudio.com/web/packages/Bchron) 
[![rstudio mirror downloads](http://cranlogs.r-pkg.org/badges/Bchron?)](https://github.com/metacran/cranlogs.app)
[![rstudio mirror downloads](http://cranlogs.r-pkg.org/badges/grand-total/Bchron?color=82b4e8)](https://github.com/metacran/cranlogs.app)

Bchron is a Bayesian chronology model implemented in R. 

The package enables quick calibration of radiocarbon dates under various calibration curves (including user generated ones); Age-depth modelling as per the algorithm of Haslett and Parnell (2008); Relative sea level rate estimation incorporating time uncertainty in polynomial regression models; and non-parametric phase modelling via Gaussian mixtures as a means to determine the activity of a site (and as an alternative to the Oxcal function SUM).

To install the development version of the package type:

```
library(devtools)
install_github('andrewcparnell/Bchron')
```

You can then explore the package with:

```
library(Bchron)
help(Bchronology) # Help on the chronology modelling function
help(BchronCalibrate) # Help on the calibrate function
```

To install the stable version of the package go to R and type:

```
install.packages('Bchron')
```
