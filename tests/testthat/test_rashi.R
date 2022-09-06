
library(testthat)
library(VedicDateTime)
source_test_helpers()

test_that("Check positive testcase", {
  expect_equal(rashi(vd$jd), 11, tolerance = .Machine$double.eps^0.4)
})
