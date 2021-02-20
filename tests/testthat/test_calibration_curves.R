library(Bchron)

test_that("intcal20 data correct", {
  data(intcal20)
  expect_true(nrow(intcal20) == 9501)
  expect_true(ncol(intcal20) == 5)
  expect_null(plot(intcal20[, 1], intcal20[, 2]))
})

test_that("intcal13 data correct", {
  data(intcal13)
  expect_true(nrow(intcal13) == 5141)
  expect_true(ncol(intcal13) == 5)
  expect_null(plot(intcal13[, 1], intcal13[, 2]))
})

test_that("marine13 data correct", {
  data(marine13)
  expect_true(nrow(marine13) == 4801)
  expect_true(ncol(marine13) == 5)
  expect_null(plot(marine13[, 1], marine13[, 2]))
})

test_that("marine20 data correct", {
  data(marine20)
  expect_true(nrow(marine20) == 5501)
  expect_true(ncol(marine20) == 5)
  expect_null(plot(marine20[, 1], marine20[, 2]))
})

test_that("shcal13 data correct", {
  data(shcal13)
  expect_true(nrow(shcal13) == 5141)
  expect_true(ncol(shcal13) == 5)
  expect_null(plot(shcal13[, 1], shcal13[, 2]))
})

test_that("shcal20 data correct", {
  data(shcal20)
  expect_true(nrow(shcal20) == 9501)
  expect_true(ncol(shcal20) == 5)
  expect_null(plot(shcal20[, 1], shcal20[, 2]))
})
