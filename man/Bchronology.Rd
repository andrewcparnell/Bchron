\name{Bchronology}
\alias{Bchronology}
\title{
Runs the Compound Poisson-Gamma chronology model of Haslett and Parnell (2008)
}
\description{
Fits a non-parametric chronology model to age/position data according to the Compound Poisson-Gamma model defined by Haslett and Parnell (2008). This version uses a slightly modified Markov chain Monte Carlo fitting algorithm which aims to converge quicker and requires fewer iterations. It also a slightly modified procedure for identifying outliers
}
\usage{
Bchronology(ages, ageSds, positions, positionThicknesses = rep(0, length(ages)), 
calCurves = rep("intcal13", length(ages)), ids = NULL, 
outlierProbs = rep(0.01, length(ages)), predictPositions = seq(min(positions), 
max(positions), length = 100), pathToCalCurves = system.file('data', 
package = "Bchron"), iterations = 10000, burn = 2000, thin = 8, 
extractDate = 1950 - as.numeric(format(Sys.time(), "\%Y")), maxExtrap = 1000, 
thetaMhSd = 0.5, muMhSd = 0.1, psiMhSd = 0.1, ageScaleVal = 1000, 
positionScaleVal = 100)
}
\arguments{
  \item{ages}{
A vector of ages (most likely 14C)
}
  \item{ageSds}{
A vector of 1-sigma values for the ages given above
}
  \item{positions}{
Position values (e.g. depths) for each age
}
  \item{positionThicknesses}{
(optional) Thickness values for each of the positions. By default set to zero
}
  \item{calCurves}{
A vector of values containing either 'intcal13', 'shcal13', 'marine13', or 'normal'. Should be the same length the number of ages supplied. Non-standard calibration curves can be used provided they are supplied in the same format as those previously mentioned and are placed in the same directory. Normal indicates a normally-distributed (non-14C) age.
}
  \item{ids}{
(optional) ID names for each age
}
  \item{outlierProbs}{
(optional) A vector of prior outlier probabilities, one for each age. Defaults to 0.01
}
  \item{predictPositions}{
(optional) A vector of positions (e.g. depths) at which predicted age values are required. Defaults to a sequence of length 100 from the top position to the bottom position 
}
  \item{pathToCalCurves}{
(optional) File path to where the calibration curves are located. Defaults to the system directory where the 3 standard calibration curves are stored. 
}
  \item{iterations}{
(optional) The number of iterations to run the procedure for
}
  \item{burn}{
(optional) The number of starting iterations to discard
}
  \item{thin}{
(optional) The step size for every iteration to keep beyond the burnin
}
  \item{extractDate}{
(optional) The top age of the core. Used for extrapolation purposes so that no extrapolated ages go beyond the top age of the core. Defaults to the current year
}
  \item{maxExtrap}{
(optional) The maximum number of extrapolations to perform before giving up and setting the predicted ages to NA. Useful for when large amounts of extrapolation are required, i.e. some of the predictPositions are a long way from the dated positions
}
  \item{thetaMhSd}{
(optional) The Metropolis-Hastings standard deviation for the age parameters
}
  \item{muMhSd}{
(optional) The Metropolis-Hastings standard deviation for the Compound Poisson-Gamma mean
}
  \item{psiMhSd}{
(optional) The Metropolis-Hastings standard deviation for the Compound Poisson-Gamma scale
}
  \item{ageScaleVal}{
(optional) A scale value for the ages. Bchronology works best when the ages are scaled to be approximately between 0 and 100. The default value is thus 1000 for ages given in years.
}
  \item{positionScaleVal}{
(optional) A scale value for the positions. Bchronology works best when the positions are scaled to be approximately between 0 and 100. The default value is thus 100 for positions given in cm.
}
}
\details{
The Bchronology function fits a compound Poisson-Gamma distribution to the increments between the dated levels. This involves a stochastic linear interpolation step where the age gaps are Gamma distributed, and the position gaps are Exponential. Radiocarbon and non-radiocarbon dates (including outliers) are updated within the function also by MCMC.
}
\value{
A list of class BchronologyRun which include elements:
\item{theta}{The posterior estimated values of the ages}
\item{phi}{The posterior estimated outlier values (1=outlier, 2=not outlier). The means of this parameter give the posterior estimated outlier probabilities}
\item{mu}{The posterior values of the Compound Poisson-Gamma mean}
\item{psi}{The posterior values of the Compound Poisson-Gamma scale}
\item{thetaPredict}{The posterior estimated ages for each of the values in predictPosition}
\item{predictPositions}{The positions at which estimated ages were required}
\item{calAges}{The calibrated ages as output from \code{\link{BchronCalibrate}}}
}
\references{
Haslett, J., and Parnell, A. C. (2008). A simple monotone process with application to radiocarbon-dated depth chronologies. Journal of the Royal Statistical Society, Series C, 57, 399-418.

Parnell, A. C., Haslett, J., Allen, J. R. M., Buck, C. E., and Huntley, B. (2008). A flexible approach to assessing synchroneity of past events using Bayesian reconstructions of sedimentation history. Quaternary Science Reviews, 27(19-20), 1872-1885.
}
\author{
Andrew Parnell <andrew.parnell@ucd.ie>
}
\seealso{
\code{\link{BchronCalibrate}}, \code{\link{BchronRSL}}, \code{\link{BchronDensity}}, \code{\link{BchronDensityFast}} 
}
\examples{
\dontrun{
# Data from Glendalough
data(Glendalough)

# Run in Bchronology - all but first age uses intcal13
GlenOut = Bchronology(ages=Glendalough$ages,ageSds=Glendalough$ageSds,
calCurves=Glendalough$calCurves,positions=Glendalough$position,
positionThicknesses=Glendalough$thickness,ids=Glendalough$id,
predictPositions=seq(0,1500,by=10))

# Summarise it a few different ways
summary(GlenOut) # Default is for quantiles of ages at predictPosition values
summary(GlenOut, type='convergence') # Check model convergence
summary(GlenOut, type='outliers') # Look at outlier probabilities

# Predict for some new positions
predictAges = predict(GlenOut, newPositions = c(150,725,1500), newPositionThicknesses=c(5,0,20))

# Plot the output
plot(GlenOut,main="Glendalough",xlab='Age (cal years BP)',ylab='Depth (cm)',las=1)}
}
