context("Other Bchron functions")

library(Bchron)


test_that('unCalibrate', {
  unCal1 = unCalibrate(2350, type = 'ages')
  unCal2 = unCalibrate(calAge = c(2350, 4750, 11440),
                       calCurve = 'shcal13',
                       type = 'ages')
  calAge = BchronCalibrate(ages = 11255,
                           ageSds = 25,
                           calCurves = 'intcal13')
  calSampleAges = sampleAges(calAge)
  unCalSamples = unCalibrate(calSampleAges,
              type = 'samples')
  expect_type(unCal1, 'double')
  expect_type(unCal2, 'double')
  expect_output(print(unCal1))
  expect_output(print(unCal2))
  expect_type(unCalSamples, 'list')
})

test_that("sampleAges", {
  ages3 = BchronCalibrate(ages=c(1000,11553), 
                          ageSds=c(50,230), 
                          positions=c(100,150), 
                          calCurves=c('intcal13','normal'))
  age_samples = sampleAges(ages3)
  expect_type(age_samples, 'double')
  expect_type(apply(age_samples, 2, quantile, prob=c(0.025,0.975)), 'double')
})

test_that("CreateCalCurve", {
  intcal09 = read.table(system.file('extdata/intcal09.14c', package = 'Bchron'), sep=',')
  expect_output(createCalCurve(name='intcal09',cal_ages=intcal09[,1], 
                 uncal_ages=intcal09[,2],one_sigma=intcal09[,3]))
})

