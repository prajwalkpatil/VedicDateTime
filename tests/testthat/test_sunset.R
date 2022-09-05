source("~/dev/VedicDateTime/tests/testthat/vedic_class.R")
library(testthat)
library(VedicDateTime)

test_that("Check positive testcase", {
  expect_equal(sunset(vd$jd, vd$place), c(2459778, 7, 2, 46), tolerance = .Machine$double.eps^0.4)
})
