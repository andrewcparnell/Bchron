\name{CreateCalCurve}
\alias{CreateCalCurve}
\title{
Create a new calibration curve
}
\description{
A function for creating a new calibration curve not already avialable in Bchron
}
\usage{
CreateCalCurve(name,cal_ages,uncal_ages,one_sigma=rep(0,length(cal_ages)))
}
\arguments{
  \item{name}{
The name of the new calibration curve
}
  \item{cal_ages}{
A vector of the calendar/calibrated ages in years before present
}
  \item{uncal_ages}{
A vector of values of uncalibrated ages in appropriate units (e.g. 14C years BP)
}
  \item{one_sigma}{
The one sigma (one standard devation) values in uncalibrated units. If left blank it assumes these are all zero
}
}
\details{
Calibration curves are stored by Bchron in the standard R gzipped text format. You can find the location of the calibration curves by typing \code{system.file('data',package='Bchron')}. Any calibration curve supplied will be converted to this format and put in the appropriate directory. It can then be used as in the examples below. However note that re-installing Bchron will likely over-write previously created calibration curves so you should make sure to store the code used to create it.
}
\references{
Forthcoming!
}
\author{
Andrew Parnell <andrew.parnell@ucd.ie>
}
\seealso{
\code{\link{BchronCalibrate}}
}
\examples{\dontrun{
# Load in the calibration curve with:
intcal09 = read.table('http://www.radiocarbon.org/IntCal09%20files/intcal09.14c',sep=',')
# Run CreateCalCurve
CreateCalCurve(name='intcal09',cal_ages=intcal09[,1],uncal_ages=intcal09[,2],one_sigma=intcal09[,3])

# Calibrate the ages under two calibration curves
age_09 = BchronCalibrate(age=15500,ageSds=150,calCurves = 'intcal09',ids='My Date')
age_13 = BchronCalibrate(age=15500,ageSds=150,calCurves = 'intcal13')

# Finally plot the difference
plot(age_09)
with(age_13$date1,lines(ageGrid,densities,col='red'))
legend('topleft',legend=c('intcal09','intcal13'),col=c('black','red'),lty=1)
}
}
