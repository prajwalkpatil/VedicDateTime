
library(testthat)
library(VedicDateTime)
source_test_helpers()

test_that("Check positive testcase", {
  expect_equal(nakshatra(vd$jd, vd$place), c(25,24,24,1), tolerance = .Machine$double.eps^0.4)
})
