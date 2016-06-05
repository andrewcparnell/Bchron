\name{SampleAges}
\alias{SampleAges}
\title{
Get sample ages from a set of Bchron calibrated dates
}
\description{
A function for extracting sample ages from Bchron calibrated dates
}
\usage{
SampleAges(CalDates, n_samp = 1000)
}
\arguments{
  \item{CalDates}{
A list created from either \code{\link{BchronCalibrate}} or \code{\link{Bchronology}}
}
  \item{n_samp}{
The desired number of samples
}
}
\details{
Sometimes it is useful to have a set of sample calendar ages for your calibrated dates. For example the samples might be required to create a credible/confidence interval, or to create another non-trivial function of calibrated dates, such as differences. By default the \code{\link{BchronCalibrate}} function provides a grid of ages and an associated density, similar to OxCal. This function extracts that information and uses the \code{\link{sample}} function to output the desired number of samples
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
\examples{
# Calibrate multiple ages and summarise them
ages = BchronCalibrate(ages=c(3445,11553,7456),ageSds=c(50,230,110),
calCurves=c('intcal13','intcal13','shcal13'))
# Get samples
age_samples = SampleAges(ages)
# Create a credible interval and the median for each date
apply(age_samples, 2, quantile, probs = c(0.05, 0.5, 0.95))
}
