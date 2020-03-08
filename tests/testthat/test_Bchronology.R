context("Bchronology and related functions")

library(Bchron)

data(Glendalough)
data(Sluggan)
data(TestChronData)
data(TestRSLData)

GlenOut = with(Glendalough, 
               Bchronology(ages=ages,
                           ageSds=ageSds, 
                           calCurves=calCurves,
                           positions=position, 
                           positionThicknesses=thickness,
                           ids=id, 
                           predictPositions=seq(0,1500,by=10)))
SlugOut = with(Sluggan,
               Bchronology(ages=ages,
                           ageSds=ageSds,
                           calCurves=calCurves,
                           positions=position,
                           positionThicknesses=thickness,
                           ids=id,
                           jitterPositions = TRUE))

predictAges1 = predict(GlenOut, 
                      newPositions = c(150,725,1500), 
                      newPositionThicknesses=c(5,0,20))
predictAges2 = predict(GlenOut, 
                      newPositions = seq(0,1500,by=10))

acc_rate = summary(GlenOut, type = 'acc_rate',
                   probs=c(0.25, 0.5, 0.75))
sed_rate = summary(GlenOut, type = 'sed_rate', useExisting = FALSE,
                   probs=c(0.25, 0.5, 0.75))

test_that('Data sets', {
  expect_output(print(Glendalough))
  expect_output(print(Sluggan))
  expect_output(print(TestChronData))
  expect_output(print(TestRSLData))
})

test_that("Bchronology", {
  expect_s3_class(GlenOut, 'BchronologyRun')
  expect_s3_class(SlugOut, 'BchronologyRun')
})

test_that("summary.BchronologyRun", {
  expect_output(summary(GlenOut, type='quantiles'))
  expect_output(summary(GlenOut, type='convergence'))
  expect_output(summary(GlenOut, type='outliers'))
  expect_output(summary(GlenOut, type='sed_rate'))
  expect_output(summary(GlenOut, type='acc_rate'))
  expect_output(summary(GlenOut, type='max_var'))
  expect_output(summary(SlugOut, type='quantiles'))
  expect_output(summary(SlugOut, type='convergence'))
  expect_output(summary(SlugOut, type='outliers'))
  expect_output(summary(SlugOut, type='sed_rate'))
  expect_output(summary(SlugOut, type='acc_rate'))
  expect_output(summary(SlugOut, type='max_var'))
})  
  
test_that("plot.BchronologyRun", {
  expect_s3_class(plot(GlenOut), 'ggplot')
  expect_s3_class(plot(SlugOut), 'ggplot')
})

test_that("predict.BchronologyRun", {
  expect_type(predictAges1, 'matrix')
  expect_type(predictAges2, 'matrix')
  expect_true(any(!is.na(predictAges1)))
  expect_true(any(!is.na(predictAges2)))
})

test_that("sedimentation and accumulation rates", {
  expect_type(acc_rate, 'data.frame')
  expect_type(sed_rate, 'data.frame')
})

test_that("Influence", {
  expect_type(dateInfluence(GlenOut, 
                whichDate = 'Beta-100901',
                measure = 'absMedianDiff'),
              'list')
  expect_type(dateInfluence(GlenOut, 
                whichDate = 'all',
                measure = 'absMedianDiff'),
              'list')
  expect_type(dateInfluence(GlenOut, 
                whichDate = 'all',
                measure = 'KL'), 'list')
})
