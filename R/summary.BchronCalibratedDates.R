summary.BchronCalibratedDates <-
function(object, prob=c(50,95,99), ..., digits = max(3, getOption("digits")-3)) {
  x = object
  for(i in 1:length(x)) {
    currdate = list(x=x[[i]]$ageGrid,y=x[[i]]$densities)
    cat('Highest density regions for',names(x)[i],'\n')
    print(round(hdrcde::hdr(den=currdate,prob=prob)$hdr,1))
    cat('\n')
  }
}
