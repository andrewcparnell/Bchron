hdr = function(date, prob = 0.95) {

# A function to return the HPD interval for a date object which should have an ageGrid and a densities argument
# I was previously using the hdrcde package but this has several unnecessary dependencies

ag = date$ageGrid
de = date$densities

# Error checking
if(is.null(ag)) stop('Age grid not found in date object.')
if(is.null(de)) stop('Densities not found in date object.')
if(findInterval(prob, c(0, 1))!=1) stop('prob value outside (0,1).')

# Put the probabilities in order
o = order(de)
cu = cumsum(de[o])

# Find which ones are above the threshold
good_cu = which(cu>1-prob)
good_ag = sort(ag[o][good_cu])

# Pick out the extremes of each range
breaks = diff(good_ag)>1
where_breaks = which(diff(good_ag)>1)
n_breaks = sum(breaks) + 1
# Store output
out = vector('list', length = n_breaks)
low_seq = 1
high_seq = ifelse(length(where_breaks)==0, length(breaks), where_breaks[1])
for(i in 1:n_breaks) {
  out[[i]] = c(good_ag[low_seq], good_ag[high_seq])
  curr_dens = round(100*sum(de[o][seq(good_cu[low_seq], good_cu[high_seq])]),1)
  names(out)[[i]] = paste0(as.character(curr_dens),'%')
  low_seq = high_seq + 1
  high_seq = ifelse(i<n_breaks-1, where_breaks[i+1], length(breaks))
}
return(out)

}