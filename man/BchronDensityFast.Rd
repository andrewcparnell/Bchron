\name{BchronDensityFast}
\alias{BchronDensityFast}
\title{
Non-parametric phase model (faster version)
}
\description{
This function runs a non-parametric phase model on 14C and non-14C ages via Gaussian Mixture density estimation through the mclust package
}
\usage{
BchronDensityFast(ages, ageSds, calCurves, pathToCalCurves = system.file('data',
package = "Bchron"), dfs = rep(100, length(ages)), samples = 2000, G = 30)
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
  \item{pathToCalCurves}{
(optional) File path to where the calibration curves are located. Defaults to the system directory where the 3 standard calibration curves are stored. 
}
  \item{dfs}{
(optional) Degrees-of-freedom values for the t-distribution associated with the calibration calculation. A large value indicates Gaussian distributions assumed for the 14C ages
}
  \item{samples}{
(optional) Number of samples of calibrated dates required
}
  \item{G}{
(optional) Number of Gaussian mixture components
}
}
\details{
This is a faster approximate version of \code{\link{BchronDensity}} that uses the \code{\link{densityMclust}} function to compute the Gaussian mixtures for a set of calibrated ages. The method is an approximation as it does not fit a fully Bayesian model as \code{\link{BchronDensity}} does. It is designed to be a probabilistic version of the Oxcal SUM command which takes calibrated ages and sums the probability distributions with the aim of estimating activity through age as a proxy.

}
\value{
An object of class BchronDensityRunFast with the following components:
\item{out}{The output from the run of \code{\link{densityMclust}} with the given number of mixture components}
\item{calAges}{The calibrated ages from the \code{\link{BchronDensity}} function}
}
\references{
See the mclust package for references about this method.
}
\author{
Andrew Parnell <andrew.parnell@ucd.ie>
}
\seealso{
\code{\link{Bchronology}}, \code{\link{BchronCalibrate}}, \code{\link{BchronRSL}}, \code{\link{BchronDensity}} for a slower exact version of this function
}
\examples{
\dontrun{# Read in some data from Sluggan Moss
data(Sluggan)

# Run the model
SlugDensFast = BchronDensityFast(ages=Sluggan$ages,ageSds=Sluggan$ageSds,
calCurves=Sluggan$calCurves)

# plot it
plot(SlugDensFast)}
}