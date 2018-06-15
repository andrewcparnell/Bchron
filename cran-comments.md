## Test environments
* local OS X install, R 3.4.3 (R 3.5.0 not in Mac Ports yet)
* win-builder (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs.

There were two NOTEs produced by devtools::release, one of which is me changing my email address as I'm moving jobs. The other is some examples which take longer than 5 seconds but these are all surrounded by \donttest

## Downstream dependencies
I have also run R CMD check on downstream dependencies of Bchron (ArchaeoChron, deltar, Bclim) and all installed fine. 

Many thanks for your continuing important work with R,

Andrew