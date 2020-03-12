context("Previously troublesome Bchronology runs")

library(Bchron)

# data(Glendalough) # Glendalough already tested in main functions

test_that('Sluggan', {
  skip_on_cran()
  skip_on_travis()
  data(Sluggan)
  expect_output(print(Sluggan))
  run = with(Sluggan, 
             Bchronology(ages=ages,
                         ageSds=ageSds, 
                         calCurves=calCurves,
                         positions=position, 
                         positionThicknesses=thickness,
                         ids=id,
                         jitterPositions = TRUE,
                         iterations = 1000,
                         burn = 200,
                         thin = 1))
  expect_s3_class(run, 'BchronologyRun')
  expect_output(summary(run, type='quantiles'))
  expect_output(summary(run, type='convergence'))
  expect_output(summary(run, type='outliers'))
  expect_output(summary(run, type='max_var'))
  expect_s3_class(plot(run), 'ggplot')
})

test_that('TestChronData', {
  skip_on_cran()
  skip_on_travis()
  data(TestChronData)
  expect_output(print(TestChronData))
  run = with(TestChronData, 
             Bchronology(ages=ages,
                         ageSds=ageSds, 
                         calCurves=calCurves,
                         positions=position, 
                         positionThicknesses=thickness,
                         ids=id,
                         jitterPositions = TRUE,
                         iterations = 1000,
                         burn = 200,
                         thin = 1))
  expect_s3_class(run, 'BchronologyRun')
  expect_output(summary(run, type='quantiles'))
  expect_output(summary(run, type='convergence'))
  expect_output(summary(run, type='outliers'))
  expect_output(summary(run, type='max_var'))
  expect_s3_class(plot(run), 'ggplot')
})

test_that('Taphocoenose_Jan20', {
  skip_on_cran()
  skip_on_travis()
  chron_df <-
    structure(
      list(
        sim_time = c(
          4750L,
          4501L,
          4001L,
          3501L,
          3001L,
          2501L,
          2001L,
          1501L,
          1001L,
          501L,
          1L
        ),
        sim_acc_rate = c(0, 1,
                         1, 1, 1, 1, 1, 1, 1, 1, 1),
        labID = c(
          "id_ 4750",
          "id_ 4501",
          "id_ 4001",
          "id_ 3501",
          "id_ 3001",
          "id_ 2501",
          "id_ 2001",
          "id_ 1501",
          "id_ 1001",
          "id_ 501",
          "id_ 1"
        ),
        sim_depth = c(
          0L,
          249L,
          749L,
          1249L,
          1749L,
          2249L,
          2749L,
          3249L,
          3749L,
          4249L,
          4749L
        ),
        sim_age = c(0,
                    249, 749, 1249, 1749, 2249, 2749, 3249, 3749, 4249, 4749),
        sim_age_round = c(0,
                          249, 749, 1249, 1749, 2249, 2749, 3249, 3749, 4249, 4749),
        error = c(10,
                  47, 62, 57, 70, 59, 64, 59, 57, 72, 69),
        calCurves = c(
          "normal",
          "normal",
          "normal",
          "normal",
          "normal",
          "normal",
          "normal",
          "normal",
          "normal",
          "normal",
          "normal"
        )
      ),
      row.names = c(NA,-11L),
      class = c("tbl_df",
                "tbl", "data.frame")
    )
  run = with(chron_df,
                   Bchronology(ages=sim_age_round,
                               ageSds=error,
                               calCurves=calCurves,
                               positions=sim_depth,
                               ids=labID,
                               iterations = 1000,
                               burn = 200,
                               thin = 1))
  expect_s3_class(run, 'BchronologyRun')
  expect_output(summary(run, type='quantiles'))
  expect_output(summary(run, type='convergence'))
  expect_output(summary(run, type='outliers'))
  expect_output(summary(run, type='max_var'))
  expect_s3_class(plot(run), 'ggplot')
})

