
library(testthat)
library(VedicDateTime)
source_test_helpers()

test_that("Check positive testcase", {
  expect_equal(sunrise(vd$jd, vd$place), c(2459779, 18, 8, 45), tolerance = .Machine$double.eps^0.4)
})
