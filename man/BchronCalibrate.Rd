\name{BchronCalibrate}
\alias{BchronCalibrate}
\title{
Fast radiocarbon calibration
}
\description{
A fast function for calibrating large numbers of radiocarbon dates involving multiple calibration curves
}
\usage{
BchronCalibrate(ages, ageSds, calCurves, ids = NULL, positions = NULL, 
pathToCalCurves = system.file('data',package = "Bchron"), 
eps = 1e-05, dfs = rep(100, length(ages)))
}
\arguments{
  \item{ages}{
A vector of ages (most likely 14C)
}
  \item{ageSds}{
A vector of 1-sigma values for the ages given above
}
  \item{calCurves}{
A vector of values containing either 'intcal13', 'shcal13', 'marine13', or 'normal'. Should be the same length the number of ages supplied. Non-standard calibration curves can be used provided they are supplied in the same format as those previously mentioned and are placed in the same directory. Normal indicates a normally-distributed (non-14C) age.
}
  \item{ids}{
(optional) ID names for each age
}
  \item{positions}{
(optional) Position values (e.g. depths) for each age
}
  \item{pathToCalCurves}{
(optional) File path to where the calibration curves are located. Defaults to the system directory where the 3 standard calibration curves are stored. 
}
  \item{eps}{
(optional) Cut-off point for density calculation. A value of eps>0 removes ages from the output which have negligible probability density
}
  \item{dfs}{
(optional) Degrees-of-freedom values for the t-distribution associated with the calibration calculation. A large value indicates Gaussian distributions assumed for the 14C ages
}
}
\details{
This function provides a direct numerical integration strategy for computing calibrated radiocarbon ages. The steps for each 14C age are approximately as follows:
1) Create a grid of ages covering the range of the calibration curve
2) Calculate the probability of each age according to the 14C age, the standard deviation supplied and the calibration curve
3) Normalise the probabilities so that they sum to 1
4) Remove any probabilities that are less than the value given for eps
Multiple calibration curves can be specified so that each 14C age can have a different curve. For ages that are not 14C, use the 'normal' calibration curve which treats the ages as normally distributed with given standard deviation
}
\value{
A list of lists where each element corresponds to a single age. Each element contains:
\item{ages}{The original age supplied}
\item{ageSDs}{The original age standard deviation supplied}
\item{positions}{The position of the age (usually the depth)}
\item{calCurves}{The calibration curve used for that age}
\item{ageGrid}{A grid of age values over which the density was created}
\item{densities}{A vector of probability values indicating the probability value for each element in ageGrid}
\item{ageLab}{The label given to the age variable}
\item{positionLab}{The label given to the position variable}
}
\references{
Forthcoming!
}
\author{
Andrew Parnell <andrew.parnell@ucd.ie>
}
\seealso{
\code{\link{Bchronology}}, \code{\link{BchronRSL}}, \code{\link{BchronDensity}}, \code{\link{BchronDensityFast}} 
}
\examples{
# Calibrate a single age
ages1 = BchronCalibrate(ages=11553,ageSds=230,calCurves='intcal13',ids='Date-1')
summary(ages1)
plot(ages1)

# Calibrate multiple ages with different calibration curves
ages2 = BchronCalibrate(ages=c(3445,11553,7456),ageSds=c(50,230,110),
calCurves=c('intcal13','intcal13','shcal13'))
summary(ages2)
plot(ages2)

# Calibrate multiple ages with multiple calibration curves and including depth
ages3 = BchronCalibrate(ages=c(3445,11553),ageSds=c(50,230),positions=c(100,150),
calCurves=c('intcal13','normal'))
summary(ages3)
plot(ages3,withDepths=TRUE)
}
