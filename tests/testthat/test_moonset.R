
library(testthat)
library(VedicDateTime)
source_test_helpers()

test_that("Check positive testcase", {
  expect_equal(moonset(vd$jd, vd$place), c(22, 31, 46), tolerance = .Machine$double.eps^0.4)
})
