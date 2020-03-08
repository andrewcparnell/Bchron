
context("BchronDensirt and related functions")

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
  expect_s3_class(SlugDensFast, 'BchronDensityRun')
})

test_that("summary.BchronologyRun", {
  expect_output(summary(SlugDens, prob = 0.95))
})
  

plot(SlugDens,xlab='Age (cal years BP)', xaxp=c(0, 16000, 16))
