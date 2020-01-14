## Test environments
* local OS X install, R 3.6.2
* rhub
* win-builder (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs.

There was one NOTE produced by devtools::release. This is due to the website radiocarbon.org currently being down which hosts some of the data sets. I'm told this will be back up again shortly (perhaps even by the time you check this package)

## Downstream dependencies
I have also run R CMD check on downstream dependencies of Bchron (ArchaeoChron, deltar, Bclim) and all installed fine. 

Many thanks for your continuing important work with R,

Andrew