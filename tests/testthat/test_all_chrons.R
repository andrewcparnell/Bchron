library(Bchron)

# data(Glendalough) # Glendalough already tested in main functions


# Sluggan -----------------------------------------------------------------


test_that("Sluggan", {
  data(Sluggan)
  expect_output(print(Sluggan))
  run <- with(
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
  )
  expect_s3_class(run, "BchronologyRun")
  expect_output(summary(run, type = "quantiles"))
  expect_output(summary(run, type = "convergence"))
  expect_output(summary(run, type = "outliers"))
  expect_output(summary(run, type = "max_var"))
  expect_s3_class(plot(run), "ggplot")
})


# TestChronData -----------------------------------------------------------


test_that("TestChronData", {
  data(TestChronData)
  expect_output(print(TestChronData))
  run <- with(
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
  )
  expect_s3_class(run, "BchronologyRun")
  expect_output(summary(run, type = "quantiles"))
  expect_output(summary(run, type = "convergence"))
  expect_output(summary(run, type = "outliers"))
  expect_output(summary(run, type = "max_var"))
  expect_s3_class(plot(run), "ggplot")
})


# Taphoncoenose -----------------------------------------------------------


test_that("Taphocoenose_Jan20", {
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
  run <- with(
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
  )
  expect_s3_class(run, "BchronologyRun")
  expect_output(summary(run, type = "quantiles"))
  expect_output(summary(run, type = "convergence"))
  expect_output(summary(run, type = "outliers"))
  expect_output(summary(run, type = "max_var"))
  expect_s3_class(plot(run), "ggplot")
})


# Kemp Jan 21 -------------------------------------------------------------

test_that("Kemp_Jan21", {
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
  run <- with(
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
  )

  expect_s3_class(run, "BchronologyRun")
  expect_output(summary(run, type = "quantiles"))
  expect_output(summary(run, type = "convergence"))
  expect_output(summary(run, type = "outliers"))
  expect_output(summary(run, type = "max_var"))
  expect_s3_class(plot(run), "ggplot")
})


# Kemp Jan 21 part 2 ------------------------------------------------------

test_that("Kemp_Jan21_part2", {
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
  run <- with(
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
  )

  expect_s3_class(run, "BchronologyRun")
  expect_output(summary(run, type = "quantiles"))
  expect_output(summary(run, type = "convergence"))
  expect_output(summary(run, type = "outliers"))
  expect_output(summary(run, type = "max_var"))
  expect_s3_class(plot(run), "ggplot")
})
