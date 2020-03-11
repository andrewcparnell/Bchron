library(testthat)
library(Bchron)

# broke up tests to allow tests to run longer than 10 min in travis
# see https://github.com/travis-ci/travis-ci/issues/3849#issuecomment-345686242
# note that if tests in test that script do not start with "test.", they will not
# run
test_check("Bchron", filter = "^test.[r-z]")