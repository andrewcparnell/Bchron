set.seed(123)
library(Bchron)
library(vdiffr)
co <- function(expr) capture.output(expr, file = "NUL")

data(Sluggan)
co(SlugDens <- BchronDensity(
  ages = Sluggan$ages,
  ageSds = Sluggan$ageSds,
  calCurves = Sluggan$calCurves,
  iterations = 100,
  burn = 20,
  thin = 1
))
co(SlugDensFast <- BchronDensityFast(
  ages = Sluggan$ages,
  ageSds = Sluggan$ageSds,
  calCurves = Sluggan$calCurves,
  samples = 100
))
co(SlugDens2 <- BchronDensity(
  ages = Sluggan$ages,
  ageSds = Sluggan$ageSds,
  calCurves = Sluggan$calCurves,
  updateAges = TRUE,
  iterations = 100,
  burn = 20,
  thin = 1
))

test_that("BchronDensity", {
  expect_s3_class(SlugDens, "BchronDensityRun")
  expect_s3_class(SlugDens2, "BchronDensityRun")
})

test_that("BchronDensityFast", {
  expect_s3_class(SlugDensFast, "BchronDensityRunFast")
  expect_error(BchronDensityFast(
    ages = Sluggan$ages[-1],
    ageSds = Sluggan$ageSds,
    calCurves = Sluggan$calCurves,
    samples = 100
  ))
  expect_error(BchronDensityFast(
    ages = Sluggan$ages,
    ageSds = Sluggan$ageSds,
    calCurves = Sluggan$calCurves[-1],
    samples = 100
  ))
})

test_that("summary.BchronDensity", {
  expect_output(summary(SlugDens, prob = 0.95))
  expect_output(summary(SlugDens2, prob = 0.95))
})

test_that("plot.BchronDensityRun", {
  p <- function() plot(SlugDens, plotRawSum = TRUE)
  expect_doppelganger("SlugDens_default", p)
  p <- function() plot(SlugDens, dateTransparency = 0.2)
  expect_doppelganger("SlugDens_transparent", p)
  p <- function() plot(SlugDens2, plotRawSum = TRUE)
  expect_doppelganger("SlugDens_rawsum", p)
  p <- function() plot(SlugDens2, dateTransparency = 0.2)
  expect_doppelganger("SlugDens2_transparent", p)
})

test_that("plot.BchronDensityRunFast", {
  p <- function() plot(SlugDensFast, plotSum = TRUE)
  expect_doppelganger("slugdensfast", p)
  p <- function() plot(SlugDensFast, dateTransparency = 0.2)
  expect_doppelganger("slugdensfast_transparent", p)
})
