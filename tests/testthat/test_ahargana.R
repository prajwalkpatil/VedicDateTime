source("~/dev/VedicDateTime/tests/testthat/vedic_class.R")
library(testthat)
library(VedicDateTime)

test_that("Check positive testcase", {
  expect_equal(ahargana(vd$jd), 1871312, tolerance = .Machine$double.eps^0.4)
})
