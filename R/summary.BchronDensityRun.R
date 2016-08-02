summary.BchronDensityRun <-
function(object,prob=0.95, ..., digits = max(3, getOption("digits")-3)) {

  out = hdr(object, prob)

  cat(paste('At probability level', prob,'there were', length(out), 'phases found.\n'))
  for(i in 1:length(out)) {
    cat(paste('Phase', i, '\n'))
    cat(paste('From', round(out[[i]][1],0), 'to', out[[i]][2], '\n'))
  }

  invisible(out)
}
