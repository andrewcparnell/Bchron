## Test environments
* local OS X install, R 3.6.2
* travis
* rhub
* win-builder (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs.

(I got a weird error from a couple of the rhub runs saying packages required were not available but this was for packages like ggplot2 so I assumed this was an rhub error)

I got one NOTE from rhub and win-builder saying:
  Days since last update: 5

Apologies for yet another update but I kept finding I was introducing bugs so decided to fully implement both testthat and codecov (95% coverage!). I promise this will be the last for a while.

## Downstream dependencies
I have also run revdepcheck on the downstream dependencies of Bchron which reported no errors. 

Many thanks for your continuing important work with R,

Andrew