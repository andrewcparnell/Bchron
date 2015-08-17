\name{plot.BchronCalibratedDates}
\alias{plot.BchronCalibratedDates}
\title{
Plot calibrated dates from a BchronCalibrate run
}
\description{
Plots calibrated radiocarbon dates from a \code{\link{BchronCalibrate}} run. Has options to plot on a position (usually depth) scale if supplied with the original run
}
\usage{
\method{plot}{BchronCalibratedDates}(x, 
withPositions = FALSE, 
xlab='Age (cal years BP)', 
ylab=ifelse(withPositions,'Position','Density'), 
pause=FALSE,
\ldots)
}
\arguments{
  \item{x}{
Output from BchronCalibrate
}
  \item{withPositions}{
Whether to plot with positions (i.e. using the position values as the y axis). Default is FALSE in which case it will produce a sequence of plots, one for each calibrated age
}
  \item{xlab}{
An x-axis label for the plot
}
  \item{ylab}{
A y-axis label for the plot
}
  \item{pause}{
Whether to pause between plots or go ahead and create them all
}
  \item{\dots}{
Other arguments to plot, see \code{\link{par}}.
}
}
\details{
These plots are intented to be pretty basic and used simply for quick information. Users are encouraged to learn the R plotting features to produce publication quality graphics
}
\references{
Forthcoming!
}
\author{
Andrew Parnell <andrew.parnell@ucd.ie>
}
\seealso{
\code{\link{BchronCalibrate}}, \code{\link{Bchronology}}, \code{\link{BchronRSL}}, \code{\link{BchronDensity}}, \code{\link{BchronDensityFast}} 
}
\examples{
# Examples in \code{\link{BchronCalibrate}}
}
