summary.BchronCalibratedDates <-
function(object, prob = 95, ..., digits = max(3, getOption("digits")-3)) {
  for(i in 1:length(object)) {
    currdate = object[[i]]
    cat('Highest density regions for',names(object)[i],'\n')

    my_hdr = hdr(currdate, prob = prob/100)
    print(my_hdr)
    cat('\n')
  }
}