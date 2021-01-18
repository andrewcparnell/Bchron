#' Summarise a Bchron density object
#'
#' Summarise a \code{\link{BchronDensity}} object
#'
#' @param object Output from a run of \code{\link{BchronDensity}}
#' @param prob Probability for identifying phases
#' @param ... Other arguments (not currently supported)
#' @param digits Number of digits to report values
#'
#' @seealso \code{\link{BchronDensity}}
#' @export
summary.BchronDensityRun <-
  function(object, prob = 0.95, ..., digits = max(3, getOption("digits") - 3)) {
    out <- hdr(object, prob)

    cat(paste("At probability level", prob, "there were", length(out), "phases found.\n"))
    for (i in 1:length(out)) {
      cat(paste("Phase", i, "\n"))
      cat(paste("From", round(out[[i]][1], 0), "to", out[[i]][2], "\n"))
    }

    invisible(out)
  }
