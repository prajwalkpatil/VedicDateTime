source("~/dev/VedicDateTime/tests/testthat/vedic_class.R")
library(testthat)
library(VedicDateTime)

test_that("Check positive testcase", {
  expect_equal(tithi(vd$jd, vd$place), c(20,20,55,35), tolerance = .Machine$double.eps^0.4)
})
