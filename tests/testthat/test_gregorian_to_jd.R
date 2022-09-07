library(testthat)
library(VedicDateTime)
source_test_helpers()

test_that("Check positive testcase", {
  expect_equal(gregorian_to_jd(17,07,2022), vd$jd, tolerance = .Machine$double.eps^0.4)
})
