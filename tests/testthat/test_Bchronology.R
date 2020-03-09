context("Bchronology and related functions")

library(Bchron)

data(Glendalough)

GlenOut = with(Glendalough, 
               Bchronology(ages=ages,
                           ageSds=ageSds, 
                           calCurves=calCurves,
                           positions=position, 
                           positionThicknesses=thickness,
                           ids=id, 
                           predictPositions=seq(0,1500,by=10)))

predictAges1 = predict(GlenOut, 
                      newPositions = c(150,725,1500), 
                      newPositionThicknesses=c(5,0,20))
predictAges2 = predict(GlenOut, 
                      newPositions = seq(0,1500,by=10))

acc_rate = summary(GlenOut, type = 'acc_rate',
                   probs=c(0.25, 0.5, 0.75))
sed_rate = summary(GlenOut, type = 'sed_rate', useExisting = FALSE,
                   probs=c(0.25, 0.5, 0.75))

# Check choosing new positions
newPositions = choosePositions(GlenOut, N = 3)
newPositions2 = choosePositions(GlenOut, N = 2,
                                positions = seq(500, 700, by = 10))

# Remove two dates
GlenOut_m2 = Bchronology(ages=Glendalough$ages[-c(3:4)],
                         ageSds=Glendalough$ageSds[-c(3:4)],
                         calCurves=Glendalough$calCurves[-c(3:4)],
                         positions=Glendalough$position[-c(3:4)],
                         positionThicknesses=Glendalough$thickness[-c(3:4)],
                         ids=Glendalough$id[-c(3:4)],
                         predictPositions=seq(0,1500,by=10))




test_that('Data sets', {
  expect_output(print(Glendalough))
})

test_that("Bchronology", {
  expect_s3_class(GlenOut, 'BchronologyRun')
})

test_that("summary.BchronologyRun", {
  expect_output(summary(GlenOut, type='quantiles'))
  expect_output(summary(GlenOut, type='convergence'))
  expect_output(summary(GlenOut, type='outliers'))
  expect_output(summary(GlenOut, type='max_var'))
})  
  
test_that("plot.BchronologyRun", {
  expect_s3_class(plot(GlenOut), 'ggplot')
})

test_that("predict.BchronologyRun", {
  expect_type(predictAges1, 'double')
  expect_type(predictAges2, 'double')
  expect_false(any(is.na(predictAges1)))
  expect_false(any(is.na(predictAges2)))
})

test_that("sedimentation and accumulation rates", {
  expect_type(acc_rate, 'list')
  expect_type(sed_rate, 'list')
})

test_that("Influence", {
  expect_type(dateInfluence(GlenOut, 
                whichDate = 'Beta-100901',
                measure = 'absMedianDiff'),
              'list')
})

test_that('choosePositions', {
  skip_on_cran()
  expect_type(newPositions, 'double')
  expect_type(newPositions2, 'double')
  expect_false(any(is.na(newPositions)))
  expect_false(any(is.na(newPositions2)))
})

test_that('coreInfluence', {
  expect_output(coreInfluence(GlenOut_m2,
                GlenOut,
                type = c('max', 'plot'),
                xlab = 'Age (cal years BP)',
                ylab = 'Depth (cm)',
                main = 'Chronology difference at 95% for
              Glendalough removing two dates',
                las = 1))
})

