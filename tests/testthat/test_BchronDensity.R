
context("BchronDensity and related functions")

library(Bchron)
library(vdiffr)

data(Sluggan)
SlugDens = BchronDensity(ages=Sluggan$ages,
                         ageSds=Sluggan$ageSds,
                         calCurves=Sluggan$calCurves)
SlugDensFast = BchronDensityFast(ages=Sluggan$ages,
                                 ageSds=Sluggan$ageSds, 
                                 calCurves=Sluggan$calCurves)
test_that("BchronDensity", {
  expect_s3_class(SlugDens, 'BchronDensityRun')
})

test_that("BchronDensity", {
  expect_s3_class(SlugDensFast, 'BchronDensityRunFast')
})

test_that("summary.BchronDensity", {
  expect_output(summary(SlugDens, prob = 0.95))
})

test_that("plot.BchronDensityRun", {
  expect_doppelganger("plot_slugDens", plot(SlugDens))
})

test_that("plot.BchronDensityRunFast", {
  expect_doppelganger("plot_slugDensFast", plot(SlugDensFast))
})
