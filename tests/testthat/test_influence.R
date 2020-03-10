context("Core and date influence metrics")

library(Bchron)

test_that("dateInfluence", {
  skip_on_cran()
  skip_on_travis()
  GlenOut = with(Glendalough, 
                 Bchronology(ages=ages,
                             ageSds=ageSds, 
                             calCurves=calCurves,
                             positions=position, 
                             positionThicknesses=thickness,
                             ids=id, 
                             predictPositions=seq(0,1500,by=100)))
  
  # Remove two dates
  GlenOut_m2 = Bchronology(ages=Glendalough$ages[-c(3:4)],
                           ageSds=Glendaloughalough$ageSds[-c(3:4)],
                           calCurves=Glendalough$calCurves[-c(3:4)],
                           positions=Glendalough$position[-c(3:4)],
                           positionThicknesses=Glendalough$thickness[-c(3:4)],
                           ids=Glendalough$id[-c(3:4)],
                           predictPositions=seq(0,1500,by=100))
  
  expect_type(dateInfluence(GlenOut, 
                            whichDate = 'Beta-100901',
                            measure = 'absMedianDiff'),
              'list')
  expect_type(dateInfluence(GlenOut, 
                            whichDate = 'Beta-100901',
                            measure = 'KL'),
              'list')
  expect_type(dateInfluence(GlenOut, 
                            whichDate = 'Beta-100901',
                            measure = 'absMeanDiff'),
              'list')
  expect_type(dateInfluence(GlenOut,
                            whichDate = 4, measure = 'absMeanDiff'),
              'list')
  expect_type(dateInfluence(GlenOut,
                            whichDate = 'all', measure = 'KL'),
              'list')
  expect_output(coreInfluence(GlenOut_m2,
                              GlenOut,
                              type = c('max', 'plot'),
                              xlab = 'Age (cal years BP)',
                              ylab = 'Depth (cm)',
                              main = 'Chronology difference at 95% for
              Glendalough removing two dates',
                              las = 1))
})