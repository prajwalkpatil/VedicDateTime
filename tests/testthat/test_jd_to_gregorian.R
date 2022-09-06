library(testthat)
library(VedicDateTime)
source_test_helpers()

test_that("Check positive testcase", {
  expect_equal(jd_to_gregorian(vd$jd), list(year=2022, month=7, day=17, hour=12), tolerance = .Machine$double.eps^0.4)
})
