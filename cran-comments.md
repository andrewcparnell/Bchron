## Test environments
* local OS X install, R 4.0.3
* travis (for the last time)
* rhub
* win-builder (devel)

## R CMD check results
There were no ERRORs or WARNINGs or NOTEs. I got some weird results from rhub to do with the package not being able to be installed on Windows Server 2008 but it passed fine with win-builder (devel)

## Downstream dependencies
I have also run revdepcheck on the downstream dependencies of Bchron which reported no errors. 

Many thanks for your continuing important work with R,

Andrew