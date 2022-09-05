source("~/dev/VedicDateTime/tests/testthat/vedic_class.R")
library(testthat)
library(VedicDateTime)

test_that("Check positive testcase", {
  expect_equal(moon_longitude(vd$jd), 346.5715, tolerance = .Machine$double.eps^0.4)
})