## Test environments
* local OS X install, R 3.6.2
* travis
* rhub
* win-builder (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs.

There's a warning on the OSX on the Bchron web page where the vignette fails but I can't interpret the error message and I cannot re-create the problem on either my own OSX build or any of the above test environments. I'm hoping this was a bug in one of the testing environments

## Downstream dependencies
I have also run revdepcheck on the downstream dependencies of Bchron which reported no errors. 

Many thanks for your continuing important work with R,

Andrew