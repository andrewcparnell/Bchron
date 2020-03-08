context("BchronCalibrate")

library(Bchron)

ages1 = BchronCalibrate(ages=11553,
                        ageSds=230,
                        calCurves='intcal13',
                        ids='Ox-123456')
ages2 = BchronCalibrate(ages=c(3445,11553,7456), 
                        ageSds=c(50,230,110), 
                        calCurves=c('intcal13','intcal13','shcal13'))
ages3 = BchronCalibrate(ages=c(1000,11553), 
                        ageSds=c(50,230), 
                        positions=c(100,150), 
                        calCurves=c('intcal13','normal'))

test_that("BchronCalibrate works", {
  expect_s3_class(ages1, 'BchronCalibratedDates')
  expect_s3_class(ages2, 'BchronCalibratedDates')
  expect_s3_class(ages3, 'BchronCalibratedDates')
})  
  
test_that("summary.BchronologyRun works", {
  expect_output(summary(ages1))
  expect_output(summary(ages2))
  expect_output(summary(ages3))
})

test_that("plot.BchronCalibrate works", {
  expect_s3_class(plot(ages1), 'ggplot')
  expect_type(plot(ages2), 'list')
  expect_s3_class(plot(ages3), 'ggplot')
})

test_that("predict.BchronCalibrate works", {
  expect_s3_class(plot(ages1), 'ggplot')
  expect_type(plot(ages2), 'list')
  expect_s3_class(plot(ages3), 'ggplot')
})

