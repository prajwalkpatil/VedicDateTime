source("~/dev/VedicDateTime/tests/testthat/vedic_class.R")
library(testthat)
library(VedicDateTime)

test_that("Check positive testcase", {
  expect_equal(masa(vd$jd, vd$place), c(4, 0), tolerance = .Machine$double.eps^0.4)
})
