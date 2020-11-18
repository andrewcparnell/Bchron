context("BchronCalibrate and related functions")

library(Bchron)

ages1 = BchronCalibrate(ages=11553,
                        ageSds=230,
                        calCurves='intcal20',
                        ids='Ox-123456')
ages2 = BchronCalibrate(ages=c(3445,11553,7456), 
                        ageSds=c(50,230,110), 
                        calCurves=c('intcal20','intcal20','shcal20'))
ages3 = BchronCalibrate(ages=c(1000,11553), 
                        ageSds=c(50,230), 
                        positions=c(100,150), 
                        calCurves=c('intcal20','normal'))

# A dodgy one that was found by a GitHub user shheidgen
ages4 = BchronCalibrate(ages = 6885, 
                        ageSds = 65, 
                        calCurve = 'intcal13')


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
  expect_s3_class(plot(ages4), 'ggplot')
  expect_s3_class(plot(ages1, includeCal = TRUE), 'ggplot')
  expect_type(plot(ages2, includeCal = TRUE), 'list')
})

test_that("predict.BchronCalibrate works", {
  expect_s3_class(plot(ages1), 'ggplot')
  expect_type(plot(ages2), 'list')
  expect_s3_class(plot(ages3), 'ggplot')
})

