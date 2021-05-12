set.seed(123)
library(Bchron)
co <- function(expr) capture.output(expr, file = "NUL")

# Test all the chronologies, including some which cause problems (and so are skipped on CI and CRAN)

# Sluggan -----------------------------------------------------------------


test_that("Sluggan", {
  data(Sluggan)
  expect_output(print(Sluggan))
  co(run <- with(
    Sluggan,
    Bchronology(
      ages = ages,
      ageSds = ageSds,
      calCurves = calCurves,
      positions = position,
      positionThicknesses = thickness,
      ids = id,
      jitterPositions = TRUE,
      iterations = 1000,
      burn = 200,
      thin = 1
    )
  ))
  expect_s3_class(run, "BchronologyRun")
  expect_output(summary(run, type = "quantiles"))
  expect_output(summary(run, type = "convergence"))
  expect_output(summary(run, type = "outliers"))
  expect_output(summary(run, type = "max_var"))
  p <- plot(run)
  expect_s3_class(p, "ggplot")
})


# TestChronData -----------------------------------------------------------


test_that("TestChronData", {
  data(TestChronData)
  expect_output(print(TestChronData))
  co(run <- with(
    TestChronData,
    Bchronology(
      ages = ages,
      ageSds = ageSds,
      calCurves = calCurves,
      positions = position,
      positionThicknesses = thickness,
      ids = id,
      jitterPositions = TRUE,
      iterations = 1000,
      burn = 200,
      thin = 1
    )
  ))
  expect_s3_class(run, "BchronologyRun")
  expect_output(summary(run, type = "quantiles"))
  expect_output(summary(run, type = "convergence"))
  expect_output(summary(run, type = "outliers"))
  expect_output(summary(run, type = "max_var"))
  p <- plot(run)
  expect_s3_class(p, "ggplot")
})


# Taphoncoenose -----------------------------------------------------------


test_that("Taphocoenose_Jan20", {
  skip_on_ci()
  skip_on_cran()
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
        sim_acc_rate = c(
          0, 1,
          1, 1, 1, 1, 1, 1, 1, 1, 1
        ),
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
        sim_age = c(
          0,
          249, 749, 1249, 1749, 2249, 2749, 3249, 3749, 4249, 4749
        ),
        sim_age_round = c(
          0,
          249, 749, 1249, 1749, 2249, 2749, 3249, 3749, 4249, 4749
        ),
        error = c(
          10,
          47, 62, 57, 70, 59, 64, 59, 57, 72, 69
        ),
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
      row.names = c(NA, -11L),
      class = c(
        "tbl_df",
        "tbl", "data.frame"
      )
    )
  co(run <- with(
    chron_df,
    Bchronology(
      ages = sim_age_round,
      ageSds = error,
      calCurves = calCurves,
      positions = sim_depth,
      ids = labID,
      iterations = 1000,
      burn = 200,
      thin = 1
    )
  ))
  expect_s3_class(run, "BchronologyRun")
  expect_output(summary(run, type = "quantiles"))
  expect_output(summary(run, type = "convergence"))
  expect_output(summary(run, type = "outliers"))
  expect_output(summary(run, type = "max_var"))
  p <- plot(run)
  expect_s3_class(p, "ggplot")
})


# Kemp Jan 21 -------------------------------------------------------------

test_that("Kemp_Jan21", {
  skip_on_ci()
  skip_on_cran()
  RC_input <- structure(list(
    id = c(
      "10373", "10374", "10375", "10376", "10517",
      "10516", "10515", "10470", "10518", "10519", "10520", "10522",
      "10521", "10523", "10524", "10525", "10526", "10527", "10528",
      "10529", "10471", "10472", "10473", "10474", "10476", "10475",
      "10477", "10478", "10479", "10480", "10481", "10482", "10483",
      "10484", "10485", "10486", "10441", "10442", "10502", "10398",
      "10399"
    ), ages = c(
      143, 176, 125, 125, 233, 286, 332, 367, 415,
      530, 546, 263, 846, 837, 1039, 1012, 1111, 1243, 1323, 1321,
      1508, 1643, 1597, 1653, 1684, 1722, 1782, 1842, 1892, 1944, 1909,
      2017, 2168, 2234, 2359, 2422, 2492, 2470, 2481, 2578, 2705
    ),
    ageSds = c(
      41, 31, 39, 35, 26, 33, 34, 33, 40, 34, 42, 29,
      38, 36, 38, 38, 30, 39, 36, 31, 31, 29, 28, 29, 30, 31, 29,
      28, 36, 30, 32, 30, 31, 33, 39, 35, 38, 43, 38, 40, 41
    ),
    position = c(
      24, 32, 40, 48, 54, 60, 66, 74, 80, 86, 94,
      102, 107, 108, 119, 125, 133, 141, 149, 157, 166, 174, 174,
      182, 189, 190, 195, 203, 208, 214, 220, 229, 235, 245, 254,
      261, 267, 271, 277, 285, 291
    ), thickness = c(
      4, 4, 4, 4,
      4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
      4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4
    ), calCurves = c(
      "intcal20",
      "intcal20", "intcal20", "intcal20", "intcal20", "intcal20",
      "intcal20", "intcal20", "intcal20", "intcal20", "intcal20",
      "intcal20", "intcal20", "intcal20", "intcal20", "intcal20",
      "intcal20", "intcal20", "intcal20", "intcal20", "intcal20",
      "intcal20", "intcal20", "intcal20", "intcal20", "intcal20",
      "intcal20", "intcal20", "intcal20", "intcal20", "intcal20",
      "intcal20", "intcal20", "intcal20", "intcal20", "intcal20",
      "intcal20", "intcal20", "intcal20", "intcal20", "intcal20"
    )
  ), class = c("spec_tbl_df", "tbl_df", "tbl", "data.frame"), row.names = c(NA, -41L), spec = structure(list(cols = list(
    id = structure(list(), class = c("collector_character", "collector")), ages = structure(list(), class = c(
      "collector_double",
      "collector"
    )), ageSds = structure(list(), class = c(
      "collector_double",
      "collector"
    )), position = structure(list(), class = c(
      "collector_double",
      "collector"
    )), thickness = structure(list(), class = c(
      "collector_double",
      "collector"
    )), calCurves = structure(list(), class = c(
      "collector_character",
      "collector"
    ))
  ), default = structure(list(), class = c(
    "collector_guess",
    "collector"
  )), skip = 1L), class = "col_spec"))

  new_error <- c(
    117, 67, 63, 69, 50, 55, 55, 59, 72, 53, 77, 41, 94, 69, 68,
    122, 59, 63, 98, 67, 57, 61, 43, 49, 89, 59, 67, 42, 104, 40,
    39, 55, 74, 82, 147, 72, 111, 85, 84, 51, 86
  )

  set.seed(344)
  co(run <- with(
    RC_input,
    Bchronology(
      ages = ages,
      ageSds = new_error,
      calCurves = calCurves,
      positions = position,
      positionThicknesses = thickness,
      ids = id,
      extractDate = -49,
      jitterPositions = TRUE,
      iterations = 1000,
      burn = 200,
      thin = 1
    )
  ))

  expect_s3_class(run, "BchronologyRun")
  expect_output(summary(run, type = "quantiles"))
  expect_output(summary(run, type = "convergence"))
  expect_output(summary(run, type = "outliers"))
  expect_output(summary(run, type = "max_var"))
  p <- plot(run)
  expect_s3_class(p, "ggplot")
})


# Kemp Jan 21 part 2 ------------------------------------------------------

test_that("Kemp_Jan21_part2", {
  skip_on_ci()
  skip_on_cran()
  RC_input <- structure(list(
    id = c(
      "10373", "10374", "10375", "10376", "10517",
      "10516", "10515", "10470", "10518", "10519", "10520", "10522",
      "10521", "10523", "10524", "10525", "10526", "10527", "10528",
      "10529", "10471", "10472", "10473", "10474", "10476", "10475",
      "10477", "10478", "10479", "10480", "10481", "10482", "10483",
      "10484", "10485", "10486", "10441", "10442", "10502", "10398",
      "10399"
    ), ages = c(
      143, 176, 125, 125, 233, 286, 332, 367, 415,
      530, 546, 263, 846, 837, 1039, 1012, 1111, 1243, 1323, 1321,
      1508, 1643, 1597, 1653, 1684, 1722, 1782, 1842, 1892, 1944, 1909,
      2017, 2168, 2234, 2359, 2422, 2492, 2470, 2481, 2578, 2705
    ),
    ageSds = c(
      41, 31, 39, 35, 26, 33, 34, 33, 40, 34, 42, 29,
      38, 36, 38, 38, 30, 39, 36, 31, 31, 29, 28, 29, 30, 31, 29,
      28, 36, 30, 32, 30, 31, 33, 39, 35, 38, 43, 38, 40, 41
    ),
    position = c(
      24, 32, 40, 48, 54, 60, 66, 74, 80, 86, 94,
      102, 107, 108, 119, 125, 133, 141, 149, 157, 166, 174, 174,
      182, 189, 190, 195, 203, 208, 214, 220, 229, 235, 245, 254,
      261, 267, 271, 277, 285, 291
    ), thickness = c(
      4, 4, 4, 4,
      4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
      4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4
    ), calCurves = c(
      "intcal20",
      "intcal20", "intcal20", "intcal20", "intcal20", "intcal20",
      "intcal20", "intcal20", "intcal20", "intcal20", "intcal20",
      "intcal20", "intcal20", "intcal20", "intcal20", "intcal20",
      "intcal20", "intcal20", "intcal20", "intcal20", "intcal20",
      "intcal20", "intcal20", "intcal20", "intcal20", "intcal20",
      "intcal20", "intcal20", "intcal20", "intcal20", "intcal20",
      "intcal20", "intcal20", "intcal20", "intcal20", "intcal20",
      "intcal20", "intcal20", "intcal20", "intcal20", "intcal20"
    )
  ), class = c("spec_tbl_df", "tbl_df", "tbl", "data.frame"), row.names = c(NA, -41L), spec = structure(list(cols = list(
    id = structure(list(), class = c("collector_character", "collector")), ages = structure(list(), class = c(
      "collector_double",
      "collector"
    )), ageSds = structure(list(), class = c(
      "collector_double",
      "collector"
    )), position = structure(list(), class = c(
      "collector_double",
      "collector"
    )), thickness = structure(list(), class = c(
      "collector_double",
      "collector"
    )), calCurves = structure(list(), class = c(
      "collector_character",
      "collector"
    ))
  ), default = structure(list(), class = c(
    "collector_guess",
    "collector"
  )), skip = 1L), class = "col_spec"))
  co(run <- with(
    RC_input,
    Bchronology(
      ages = ages,
      ageSds = ageSds,
      calCurves = calCurves,
      positions = position,
      positionThicknesses = thickness,
      ids = id,
      extractDate = -49,
      jitterPositions = TRUE,
      iterations = 1000,
      burn = 200,
      thin = 1
    )
  ))

  expect_s3_class(run, "BchronologyRun")
  expect_output(summary(run, type = "quantiles"))
  expect_output(summary(run, type = "convergence"))
  expect_output(summary(run, type = "outliers"))
  expect_output(summary(run, type = "max_var"))
  p <- plot(run)
  expect_s3_class(p, "ggplot")
})


# New Bchron problem - #17 issue ------------------------------------------

test_that("Gregor_Github17_20210408", {
  skip_on_ci()
  skip_on_cran()

  set.seed(210308)
  Bchron_Frame <- structure(list(id = c(
    "Co1412 0", "Co1412 51.5", "Co1412 98.5",
    "Co1412 168.6", "Co1412 253.5", "Co1412 253.5", "Co1412 258.5",
    "Co1412 258.5", "Co1412 279.5", "Co1412 286.5", "Co1412 306",
    "Co1412 345.5", "Co1412 386.5", "Co1412 416", "Co1412 465", "Co1412 465",
    "Co1412 502.5"
  ), ages = c(
    -67L, 4695L, 9269L, 14592L, 19804L,
    27720L, 45423L, 25750L, 31375L, 44198L, 45769L, 32400L, 39299L,
    48128L, 49559L, 39810L, 46886L
  ), ageSds = c(
    5L, 167L, 285L, 540L,
    1026L, 140L, 1480L, 180L, 238L, 442L, 363L, 220L, 321L, 2304L,
    2402L, 410L, 1762L
  ), position = c(
    0, 51.5, 98.5, 168.6, 253.5,
    253.5, 258.5, 258.5, 279.5, 286.5, 306, 345.5, 386.5, 416, 465,
    465, 502.5
  ), thickness = c(
    0L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L,
    1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L
  ), calCurves = c(
    "normal", "intcal20",
    "intcal20", "intcal20", "intcal20", "intcal20", "intcal20", "intcal20",
    "intcal20", "intcal20", "intcal20", "intcal20", "intcal20", "intcal20",
    "intcal20", "intcal20", "intcal20"
  )), class = "data.frame", row.names = c(
    NA,
    -17L
  ))

  # test <- BchronCalibrate(ages = 46886L, ageSds = 1762L)
  # plot(test, includeCal = TRUE, dateHeight = 500)

  co(run <- Bchronology(
    ages = Bchron_Frame$ages,
    ageSds = Bchron_Frame$ageSds,
    calCurves = Bchron_Frame$calCurves,
    positions = Bchron_Frame$position,
    positionThickness = Bchron_Frame$thickness,
    ids = Bchron_Frame$id,
    jitterPositions = TRUE,
    iterations = 1500,
    burn = 500,
    thin = 1,
    predictPositions = seq(min(Bchron_Frame$position), max(Bchron_Frame$position), by = 1)
  ))
  expect_s3_class(run, "BchronologyRun")
  expect_output(summary(run, type = "quantiles"))
  expect_output(summary(run, type = "convergence"))
  expect_output(summary(run, type = "outliers"))
  expect_output(summary(run, type = "max_var"))
  p <- plot(run)
  expect_s3_class(p, "ggplot")
})

# New Bchron problem - #17 issue - part 2 ------------------------------------------

test_that("Gregor_Github17_20210408_b", {
  skip_on_ci()
  skip_on_cran()

  set.seed(210308)
  Bchron_Frame <- structure(list(id = c(
    "PG1975 0", "PG1975 0.25", "PG1975 0.25",
    "PG1975 44.75", "PG1975 44.75", "PG1975 90.25", "PG1975 90.25",
    "PG1975 134.5", "PG1975 134.5"
  ), ages = c(
    -59L, 2980L, 2980L,
    7090L, 6190L, 6240L, 5740L, 9580L, 6790L
  ), ageSds = c(
    5L, 35L,
    35L, 50L, 40L, 50L, 40L, 35L, 30L
  ), position = c(
    0, 0.25, 0.25,
    44.75, 44.75, 90.25, 90.25, 134.5, 134.5
  ), thickness = c(
    0, 0.5,
    0.5, 0.5, 0.5, 0.5, 0.5, 1, 1
  ), calCurves = c(
    "normal", "intcal20",
    "intcal20", "intcal20", "intcal20", "intcal20", "intcal20", "intcal20",
    "intcal20"
  )), class = "data.frame", row.names = c(NA, -9L))

  # test <- BchronCalibrate(ages = 46886L, ageSds = 1762L)
  # plot(test, includeCal = TRUE, dateHeight = 500)

  co(run <- Bchronology(
    ages = Bchron_Frame$ages,
    ageSds = Bchron_Frame$ageSds,
    calCurves = Bchron_Frame$calCurves,
    positions = Bchron_Frame$position,
    positionThickness = Bchron_Frame$thickness,
    ids = Bchron_Frame$id,
    jitterPositions = TRUE,
    iterations = 1500,
    burn = 500,
    thin = 1,
    predictPositions = seq(min(Bchron_Frame$position), max(Bchron_Frame$position), by = 1)
  ))
  expect_s3_class(run, "BchronologyRun")
  expect_output(summary(run, type = "quantiles"))
  expect_output(summary(run, type = "convergence"))
  expect_output(summary(run, type = "outliers"))
  expect_output(summary(run, type = "max_var"))
})

# New Bchron problem - #17 issue - part 3 ------------------------------------------

test_that("Gregor_Github17_20210408_b", {
  skip_on_ci()
  skip_on_cran()

  set.seed(-1673826857L)
  Bchron_Frame <- structure(list(id = c(
    "PG1755 0", "PG1755 37.5", "PG1755 57.5",
    "PG1755 78.75", "PG1755 108.5", "PG1755 128.5", "PG1755 133",
    "PG1755 151", "PG1755 155", "PG1755 168.5", "PG1755 181", "PG1755 194.5",
    "PG1755 197", "PG1755 199.5", "PG1755 214", "PG1755 214.5", "PG1755 249.25",
    "PG1755 287", "PG1755 291", "PG1755 424", "PG1755 532", "PG1755 599",
    "PG1755 707", "PG1755 725", "PG1755 750", "PG1755 770", "PG1755 811",
    "PG1755 842", "PG1755 854", "PG1755 899.5", "PG1755 915", "PG1755 934"
  ), ages = c(
    -55L, 3500L, 4429L, 5698L, 8296L, 10150L, 9450L,
    18150L, 14339L, 16627L, 33688L, 17999L, 18680L, 18172L, 21490L,
    18954L, 19267L, 22960L, 20969L, 25207L, 27220L, 30610L, 30400L,
    42400L, 43000L, 41632L, 42121L, 52300L, 41436L, 37949L, 47300L,
    36140L
  ), ageSds = c(
    5L, 47L, 47L, 48L, 49L, 50L, 40L, 120L, 54L,
    57L, 150L, 59L, 120L, 59L, 110L, 63L, 62L, 230L, 68L, 82L, 200L,
    119L, 500L, 375L, 900L, 344L, 359L, 3100L, 335L, 236L, 1700L,
    197L
  ), position = c(
    0, 37.5, 57.5, 78.75, 108.5, 128.5, 133,
    151, 155, 168.5, 181, 194.5, 197, 199.5, 214, 214.5, 249.25,
    287, 291, 424, 532, 599, 707, 725, 750, 770, 811, 842, 854, 899.5,
    915, 934
  ), thickness = c(
    0, 0.5, 0.5, 0.5, 0.5, 0.5, 1, 1, 0.5,
    0.5, 0.5, 0.5, 1, 0.5, 1, 0.5, 0.5, 1, 0.5, 0.5, 1, 0.5, 1, 0.5,
    1, 0.5, 0.5, 1, 0.5, 0.5, 1, 0.5
  ), calCurves = c(
    "normal", "intcal20",
    "intcal20", "intcal20", "intcal20", "intcal20", "intcal20", "intcal20",
    "intcal20", "intcal20", "intcal20", "intcal20", "intcal20", "intcal20",
    "intcal20", "intcal20", "intcal20", "intcal20", "intcal20", "intcal20",
    "intcal20", "intcal20", "intcal20", "intcal20", "intcal20", "intcal20",
    "intcal20", "normal", "intcal20", "intcal20", "intcal20", "intcal20"
  )), class = "data.frame", row.names = c(NA, -32L))
  # Bchron_Frame = Bchron_Frame %>% arrange(desc(position))
  # test <- BchronCalibrate(ages = 46886L, ageSds = 1762L)
  # plot(test, includeCal = TRUE, dateHeight = 500)

  co(run <- Bchronology(
    ages = Bchron_Frame$ages,
    ageSds = Bchron_Frame$ageSds,
    calCurves = Bchron_Frame$calCurves,
    positions = Bchron_Frame$position,
    positionThickness = Bchron_Frame$thickness,
    ids = Bchron_Frame$id,
    jitterPositions = TRUE,
    iterations = 1500,
    burn = 500,
    thin = 1,
    predictPositions = seq(min(Bchron_Frame$position), max(Bchron_Frame$position), by = 1)
  ))
  expect_s3_class(run, "BchronologyRun")
  expect_output(summary(run, type = "quantiles"))
  expect_output(summary(run, type = "convergence"))
  expect_output(summary(run, type = "outliers"))
  expect_output(summary(run, type = "max_var"))
})

# New Bchron problem - #17 issue - part 3 ------------------------------------------

test_that("Gregor_Github17_20210408_c", {
  skip_on_ci()
  skip_on_cran()

  set.seed(10407L)
  Bchron_Frame <- structure(list(id = c(
    "PG1238 0", "PG1238 14", "PG1238 20.8",
    "PG1238 200.8", "PG1238 261", "PG1238 307.8", "PG1238 361", "PG1238 613",
    "PG1238 714.8", "PG1238 773", "PG1238 773", "PG1238 811.8", "PG1238 841.8",
    "PG1238 863", "PG1238 885", "PG1238 926", "PG1238 962.8", "PG1238 965",
    "PG1238 994.75", "PG1238 996.8", "PG1238 1005.75"
  ), ages = c(
    -46L,
    10922L, 4400L, 4030L, 8189L, 5120L, 9253L, 6020L, 12110L, 11377L,
    18434L, 19200L, 20500L, 24170L, 22953L, 27400L, 33400L, 25570L,
    34000L, 5300L, 84000L
  ), ageSds = c(
    5L, 153L, 380L, 420L, 354L,
    680L, 71L, 100L, 680L, 85L, 118L, 1300L, 910L, 160L, 161L, 220L,
    2100L, 220L, 4000L, 3000L, 6000L
  ), position = c(
    0, 14, 20.8,
    200.8, 261, 307.8, 361, 613, 714.8, 773, 773, 811.8, 841.8, 863,
    885, 926, 962.8, 965, 994.75, 996.8, 1005.75
  ), thickness = c(
    0,
    6, 1, 1, 6, 1, 2, 6, 1, 6, 6, 1, 1, 2, 6, 4, 1, 6, 9.5, 1, 10.5
  ), calCurves = c(
    "normal", "intcal20", "normal", "normal", "intcal20",
    "normal", "intcal20", "intcal20", "normal", "intcal20", "intcal20",
    "normal", "normal", "marine20", "intcal20", "marine20", "normal",
    "marine20", "normal", "normal", "normal"
  )), class = "data.frame", row.names = c(
    NA,
    -21L
  ))
  # Bchron_Frame = Bchron_Frame %>% arrange(desc(position))
  # test <- BchronCalibrate(ages = 46886L, ageSds = 1762L)
  # plot(test, includeCal = TRUE, dateHeight = 500)

  co(run <- Bchronology(
    ages = Bchron_Frame$ages,
    ageSds = Bchron_Frame$ageSds,
    calCurves = Bchron_Frame$calCurves,
    positions = Bchron_Frame$position,
    positionThickness = Bchron_Frame$thickness,
    ids = Bchron_Frame$id,
    jitterPositions = TRUE,
    iterations = 1500,
    burn = 500,
    thin = 1,
    predictPositions = seq(min(Bchron_Frame$position), max(Bchron_Frame$position), by = 1)
  ))
  expect_s3_class(run, "BchronologyRun")
  expect_output(summary(run, type = "quantiles"))
  expect_output(summary(run, type = "convergence"))
  expect_output(summary(run, type = "outliers"))
  expect_output(summary(run, type = "max_var"))
})

# New Bchron problem - #17 issue - part 4 ------------------------------------------

test_that("Gregor_Github17_20210510_a", {
  skip_on_ci()
  skip_on_cran()

  set.seed(-1673826857L)
  Bchron_Frame <- structure(list(id = c(
    "ESM-1 0", "ESM-1 27.75", "ESM-1 51.75",
    "ESM-1 83.75", "ESM-1 114.5", "ESM-1 149.5", "ESM-1 184.5", "ESM-1 209.5",
    "ESM-1 244.5", "ESM-1 279.5", "ESM-1 304.5", "ESM-1 329.5", "ESM-1 359.5",
    "ESM-1 370.5"
  ), ages = c(
    -56L, 520L, 620L, 905L, 1720L, 2145L,
    2500L, 2720L, 3380L, 3755L, 4700L, 6810L, 8810L, 9990L
  ), ageSds = c(
    5L,
    30L, 30L, 30L, 35L, 35L, 35L, 35L, 40L, 35L, 40L, 50L, 60L, 60L
  ), position = c(
    0, 27.75, 51.75, 83.75, 114.5, 149.5, 184.5,
    209.5, 244.5, 279.5, 304.5, 329.5, 359.5, 370.5
  ), thickness = c(
    0,
    0.5, 0.5, 0.5, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
  ), calCurves = c(
    "normal",
    "intcal20", "intcal20", "intcal20", "intcal20", "intcal20", "intcal20",
    "intcal20", "intcal20", "intcal20", "intcal20", "intcal20", "intcal20",
    "intcal20"
  )), class = "data.frame", row.names = c(NA, -14L))
  # Bchron_Frame = Bchron_Frame %>% arrange(desc(position))
  # test <- with(Bchron_Frame, BchronCalibrate(ages = ages, ageSds = ageSds, calCurves = calCurves, positions = position))
  # plot(test, withPositions = TRUE)

  co(run <- Bchronology(
    ages = Bchron_Frame$ages,
    ageSds = Bchron_Frame$ageSds,
    calCurves = Bchron_Frame$calCurves,
    positions = Bchron_Frame$position,
    positionThickness = Bchron_Frame$thickness,
    ids = Bchron_Frame$id,
    jitterPositions = TRUE,
    iterations = 1500,
    burn = 500,
    thin = 1,
    predictPositions = seq(min(Bchron_Frame$position), max(Bchron_Frame$position), by = 1)
  ))
  expect_s3_class(run, "BchronologyRun")
  expect_output(summary(run, type = "quantiles"))
  expect_output(summary(run, type = "convergence"))
  expect_output(summary(run, type = "outliers"))
  expect_output(summary(run, type = "max_var"))
})

# New Bchron problem - #17 issue - part 5675345243 ------------------------------------------

test_that("Gregor_Github17_20210510_b", {
  skip_on_ci()
  skip_on_cran()

  set.seed(10407L)
  Bchron_Frame <- structure(list(id = c(
    "PG1975 0", "PG1975 0.25", "PG1975 0.25",
    "PG1975 44.75", "PG1975 44.75", "PG1975 90.25", "PG1975 90.25",
    "PG1975 134.5", "PG1975 134.5"
  ), ages = c(
    -59L, 2980L, 2980L,
    7090L, 6190L, 6240L, 5740L, 9580L, 6790L
  ), ageSds = c(
    5L, 35L,
    35L, 50L, 40L, 50L, 40L, 35L, 30L
  ), position = c(
    0, 0.25, 0.25,
    44.75, 44.75, 90.25, 90.25, 134.5, 134.5
  ), thickness = c(
    0, 0.5,
    0.5, 0.5, 0.5, 0.5, 0.5, 1, 1
  ), calCurves = c(
    "normal", "intcal20",
    "intcal20", "intcal20", "intcal20", "intcal20", "intcal20", "intcal20",
    "intcal20"
  )), class = "data.frame", row.names = c(NA, -9L))
  # test <- with(Bchron_Frame, BchronCalibrate(ages = ages, ageSds = ageSds, calCurves = calCurves, positions = position))
  # plot(test, withPositions = TRUE)

  co(run <- Bchronology(
    ages = Bchron_Frame$ages,
    ageSds = Bchron_Frame$ageSds,
    calCurves = Bchron_Frame$calCurves,
    positions = Bchron_Frame$position,
    positionThickness = Bchron_Frame$thickness,
    ids = Bchron_Frame$id,
    jitterPositions = TRUE,
    iterations = 1500,
    burn = 500,
    thin = 1,
    predictPositions = seq(min(Bchron_Frame$position), max(Bchron_Frame$position), by = 1)
  ))
  expect_s3_class(run, "BchronologyRun")
  expect_output(summary(run, type = "quantiles"))
  expect_output(summary(run, type = "convergence"))
  expect_output(summary(run, type = "outliers"))
  expect_output(summary(run, type = "max_var"))

  # And again with a different seed
  set.seed(-769196902L)
  co(run <- Bchronology(
    ages = Bchron_Frame$ages,
    ageSds = Bchron_Frame$ageSds,
    calCurves = Bchron_Frame$calCurves,
    positions = Bchron_Frame$position,
    positionThickness = Bchron_Frame$thickness,
    ids = Bchron_Frame$id,
    jitterPositions = TRUE,
    iterations = 1500,
    burn = 500,
    thin = 1,
    predictPositions = seq(min(Bchron_Frame$position), max(Bchron_Frame$position), by = 1)
  ))
  expect_s3_class(run, "BchronologyRun")
  expect_output(summary(run, type = "quantiles"))
  expect_output(summary(run, type = "convergence"))
  expect_output(summary(run, type = "outliers"))
  expect_output(summary(run, type = "max_var"))
})
