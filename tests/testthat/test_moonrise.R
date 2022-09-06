
library(testthat)
library(VedicDateTime)
source_test_helpers()

test_that("Check positive testcase", {
  expect_equal(moonrise(vd$jd, vd$place), c(10,26,24), tolerance = .Machine$double.eps^0.4)
})
