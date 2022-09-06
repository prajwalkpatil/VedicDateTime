library(testthat)
library(VedicDateTime)
source_test_helpers()

test_that("Check positive testcase", {
  expect_equal(ahargana(vd$jd), 1871312, tolerance = .Machine$double.eps^0.4)
})
