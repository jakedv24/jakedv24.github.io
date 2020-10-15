source("./workingFile.R", chdir = FALSE)
library(testthat)

test_that("value", {
  x = convertTimeToDecimal("1:30")
  expect_equal(x, 1.5)
})

test_that("value", {
  x = convertTimeToDecimal("1:50")
  expect_equal(x, 1.83, tolerance = .01)
})

test_that("value", {
  x = convertTimeToDecimal("5:00")
  expect_equal(x, 5.0)
})