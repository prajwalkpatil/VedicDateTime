
library(testthat)
library(VedicDateTime)
source_test_helpers()

test_that("Check positive testcase", {
  expect_equal(yoga(vd$jd, vd$place), c(5,27,26,12), tolerance = .Machine$double.eps^0.4)
})
