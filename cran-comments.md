## Test environments
* local OS X install, R 3.6.2
* rhub
* win-builder (devel)

## R CMD check results
There were no ERRORs or WARNINGs.

For R-hub there was one NOTE: 
checking for non-standard things in the check directory ... NOTE
    'Bchron-Ex_i386.Rout' 'Bchron-Ex_x64.Rout' 'examples_i386'
  Found the following files/directories:
    'examples_x64'
    
I can't see why this might have occurred and can't find any solution online to fix this. I'd be grateful for any help.

There was one NOTE produced by devtools::check_win_devel. This is due to the website radiocarbon.org currently being down which hosts some of the data sets. I'm told this will be back up again shortly.

## Downstream dependencies
I have also run install.packages on the 4 downstream dependencies of Bchron (ArchaeoChron, deltar, Bclim, c14bazAAR) and all installed fine. 

Many thanks for your continuing important work with R,

Andrew