set.seed(123)
library(Bchron)
co <- function(expr) capture.output(expr, file = NULL)

# Test whether changing the order of the chronologies causes problems

# Start by running standard BChronology run on Glendalough, then change the order 
# of the data and check that the results are the same
# Needs to be full runs so a bit slow

data(Glendalough)

set.seed(100)

co(GlenOut1 <- with(
  Glendalough,
  Bchronology(
    ages = ages,
    ageSds = ageSds,
    calCurves = calCurves,
    positions = position,
    positionThicknesses = thickness,
    ids = id,
    predictPositions = seq(-10, 1500, by = 10),
  )
))

# Shuffle Glendalough2 rows in a random order
random_order <- sample(1:nrow(Glendalough))
Glendalough2 <- Glendalough[random_order, ]

co(GlenOut2 <- with(
  Glendalough2,
  Bchronology(
    ages = ages,
    ageSds = ageSds,
    calCurves = calCurves,
    positions = position,
    positionThicknesses = thickness,
    ids = id,
    predictPositions = seq(-10, 1500, by = 10),
  )
))

# First check it ran
test_that("Did it run?", {
  expect_output(print(GlenOut1))
  expect_output(print(GlenOut2))
})

# Tolerance requirement 1% difference ok?
my_tol <- 1e-2

test_that("summary.BchronologyRun", {
  expect_output(summary(GlenOut1, type = "quantiles"))
  expect_output(summary(GlenOut2, type = "quantiles"))
  # Test how much the medians differ
  co(med1 <- summary(GlenOut1, type = "quantiles")[,4])
  co(med2 <- summary(GlenOut2, type = "quantiles")[,4])
  expect_equal(med1, med2, tolerance = my_tol)
})
