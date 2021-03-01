set.seed(123)
library(Bchron)
library(vdiffr)
library(ggplot2)
co <- function(expr) capture.output(expr, file = "NUL")

data(TestChronData)
data(TestRSLData)

co(RSLchron <- with(
  TestChronData,
  Bchronology(
    ages = ages,
    ageSds = ageSds,
    positions = position,
    positionThicknesses = thickness,
    ids = id,
    calCurves = calCurves,
    jitterPositions = TRUE,
    predictPositions = TestRSLData$Depth,
    iterations = 100,
    burn = 20,
    thin = 1
  )
))
co(RSLrun <- with(
  TestRSLData,
  BchronRSL(RSLchron,
    RSLmean = RSL,
    RSLsd = Sigma,
    degree = 3,
    iterations = 100,
    burn = 20,
    thin = 1
  )
))

test_that("RSL Bchronology", {
  expect_s3_class(RSLchron, "BchronologyRun")
})

test_that("RSL functions", {
  expect_s3_class(RSLrun, "BchronRSLRun")
})

test_that("summary and plot RSL functions", {
  p <- plot(RSLrun, type = "RSL") + ggtitle("Relative sea level plot")
  expect_doppelganger("RSL_p1", p)
  p <- plot(RSLrun, type = "rate") + ggtitle("Rate of RSL change") +
    ylab("Rate (mm per year)")
  expect_doppelganger("RSL_p2", p)
  p <- plot(RSLrun, type = "accel") + ggtitle("Rate of RSL change") +
    ylab("Rate (mm per year)")
  expect_doppelganger("RSL_p3", p)
  expect_output(summary(RSLrun, type = "RSL", age_grid = seq(0, 2000, by = 250)))
  expect_output(summary(RSLrun, type = "parameters", age_grid = seq(0, 2000, by = 250)))
  expect_output(summary(RSLrun, type = "rate", age_grid = seq(0, 2000, by = 250)))
  expect_output(summary(RSLrun, type = "accel", age_grid = seq(0, 2000, by = 250)))
})
