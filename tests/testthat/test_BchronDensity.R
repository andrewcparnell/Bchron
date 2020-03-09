
context("BchronDensity and related functions")

library(Bchron)

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

test_that("summary.BchronologyRun", {
  expect_output(summary(SlugDens, prob = 0.95))
})
  