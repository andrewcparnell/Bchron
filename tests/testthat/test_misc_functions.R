context("Other Bchron functions")

library(Bchron)

unCal1 = unCalibrate(2350, type = 'ages')
unCal2 = unCalibrate(calAge = c(2350, 4750, 11440),
                     calCurve = 'shcal13',
                     type = 'ages')
ages3 = BchronCalibrate(ages=c(1000,11553), 
                        ageSds=c(50,230), 
                        positions=c(100,150), 
                        calCurves=c('intcal13','normal'))
age_samples = sampleAges(ages3)

test_that('unCalibrate', {
  expect_type(unCal1, 'double')
  expect_type(unCal2, 'double')
  expect_output(print(unCal1))
  expect_output(print(unCal2))
})

test_that("sampleAges", {
  expect_type(age_samples, 'double')
  expect_type(apply(age_samples, 2, quantile, prob=c(0.025,0.975)), 'double')
})


