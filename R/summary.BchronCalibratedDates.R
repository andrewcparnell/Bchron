#' Summarise a BchronCalibrate object
#'
#' Produces summary output from a \code{\link{BchronCalibrate}} run, including the highest density regions for the calibrated ages for given probability levels
#'
#' @param object The output of a run of \code{\link{BchronCalibrate}}
#' @param prob A percentage value (between 0 and 100) at which the highest density regions for each age are calculated
#' @param ... Further arguments (not currently supported)
#' @param digits Significant digits to display (not currently supported)
#'
#' @seealso \code{\link{BchronCalibrate}}, \code{\link{Bchronology}}, \code{\link{BchronRSL}}, \code{\link{BchronDensity}}, \code{\link{BchronDensityFast}}
#'
#' @export
summary.BchronCalibratedDates <-
function(object, prob = 95, ..., digits = max(3, getOption("digits")-3)) {
  out = vector('list', length=length(object))
  for(i in 1:length(object)) {
    currdate = object[[i]]
    cat(prob, '% Highest density regions for ',names(object)[i],'\n', sep = '')

    out[[i]] = hdr(currdate, prob = prob/100)
    print(out[[i]])
    cat('\n')
  }
  invisible(out)
}