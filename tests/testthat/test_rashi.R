source("~/dev/VedicDateTime/tests/testthat/vedic_class.R")
library(testthat)
library(VedicDateTime)

test_that("Check positive testcase", {
  expect_equal(rashi(vd$jd), 11, tolerance = .Machine$double.eps^0.4)
})
