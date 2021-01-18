
context("BchronDensity and related functions")

library(Bchron)

data(Sluggan)
SlugDens <- BchronDensity(
  ages = Sluggan$ages,
  ageSds = Sluggan$ageSds,
  calCurves = Sluggan$calCurves,
  iterations = 100,
  burn = 20,
  thin = 1
)
SlugDensFast <- BchronDensityFast(
  ages = Sluggan$ages,
  ageSds = Sluggan$ageSds,
  calCurves = Sluggan$calCurves,
  samples = 100
)
test_that("BchronDensity", {
  expect_s3_class(SlugDens, "BchronDensityRun")
})

test_that("BchronDensity", {
  expect_s3_class(SlugDensFast, "BchronDensityRunFast")
})

test_that("summary.BchronDensity", {
  expect_output(summary(SlugDens, prob = 0.95))
})

test_that("plot.BchronDensityRun", {
  expect_null(plot(SlugDens, plotRawSum = TRUE))
  expect_null(plot(SlugDens, dateTransparency = 0.2))
})

test_that("plot.BchronDensityRunFast", {
  expect_null(plot(SlugDensFast, plotSum = TRUE))
  expect_null(plot(SlugDensFast, dateTransparency = 0.2))
})
