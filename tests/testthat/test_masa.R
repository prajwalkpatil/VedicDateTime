
library(testthat)
library(VedicDateTime)
source_test_helpers()

test_that("Check positive testcase", {
  expect_equal(masa(vd$jd, vd$place), c(4, 0), tolerance = .Machine$double.eps^0.4)
})
