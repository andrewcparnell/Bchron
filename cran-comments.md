## Test environments
* local OS X install, R 3.6.2
* travis
* rhub
* win-builder (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs.

There was one NOTE from rhub saying:
Found the following files/directories:
    'Bchron-Ex_i386.Rout' 'Bchron-Ex_x64.Rout' 'examples_i386'
    'examples_x64'

I don't know what to do with this and can't find any other help online about it. Hopefully just an rhub problem?

## Downstream dependencies
I have also run revdepcheck on the 4 downstream dependencies of Bchron (ArchaeoChron, deltar, Bclim, c14bazAAR) and all installed fine. 

Many thanks for your continuing important work with R,

Andrew