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