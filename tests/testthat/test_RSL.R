context("RSL functions")

library(Bchron)
library(ggplot2)
data(TestChronData)
data(TestRSLData)

RSLchron = with(TestChronData, 
                Bchronology(ages = ages,
                            ageSds = ageSds,
                            positions = position,
                            positionThicknesses = thickness,
                            ids = id,
                            calCurves = calCurves,
                            jitterPositions = TRUE,
                            predictPositions = TestRSLData$Depth))
RSLrun = with(TestRSLData, 
              BchronRSL(RSLchron,
                        RSLmean = RSL,
                        RSLsd = Sigma,
                        degree = 3))

test_that("RSL Bchronology", {
  expect_s3_class(RSLchron, 'BchronologyRun')
})

test_that("RSL functions", {
  expect_s3_class(RSLrun, 'BchronRSLRun')
})

test_that("summary and plot RSL functions", {
  expect_s3_class(plot(RSLrun, type = 'RSL') + ggtitle('Relative sea level plot'),
                  'ggplot')
  expect_s3_class(plot(RSLrun, type = 'rate') + ggtitle('Rate of RSL change') + 
                    ylab('Rate (mm per year)'), 'ggplot')
  expect_s3_class(plot(RSLrun, type = 'accel') + ggtitle('Rate of RSL change') + 
                    ylab('Rate (mm per year)'), 'ggplot')
  expect_output(summary(RSLrun, type = 'RSL', age_grid = seq(0, 2000, by  = 250)))
  expect_output(summary(RSLrun, type = 'parameters', age_grid = seq(0, 2000, by  = 250)))
  expect_output(summary(RSLrun, type = 'rate', age_grid = seq(0, 2000, by  = 250)))
  expect_output(summary(RSLrun, type = 'accel', age_grid = seq(0, 2000, by  = 250)))
})


