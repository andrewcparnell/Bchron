## Test environments
* local OS X install, R 4.5.0
* GitHub Actions
* win-builder (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs on any of the above

On my local machine I got a NOTE about being unable to verify the time but I don't think I can fix this. 

On win-builder I got a NOTE about some possibly mis-spelled words which aren't mis-spelled.

Following the advice in an email from CRAN moderators I have:
- Reduced the size of the title down to <65 characters
- Removed the line breaks in the Description
- Added single quotes and brackets around packages names and functions in the description
- Added \value fields to the functions missing them in the Rd files. 
- Change dontrun to donttest in the examples
- Removed all print/cat commands from functions (except for those which are summary/print functions)
- Stopped one of my functions from writing a file to the working directory, instead writing it to tempdir()

Since implementing these changes I have re-checked the package locally, using Github Actions, and using win-builder

Many thanks for your continuing important work with R,

Andrew