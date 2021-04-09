## Test environments
* local OS X install, R 4.0.5
* GitHub Actions
* rhub
* win-builder (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs or NOTEs. Some of the Rhub runs failed to complete but this looked like a problem with some vdiffr tests not passing (which do pass on my local machine) rather than with the package

## Downstream dependencies
I have also run revdepcheck on the downstream dependencies of Bchron which reported no errors. 

Many thanks for your continuing important work with R,

Andrew