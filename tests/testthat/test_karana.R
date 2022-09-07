library(testthat)
library(VedicDateTime)
source_test_helpers()

test_that("Check positive testcase", {
  expect_equal(karana(vd$jd, vd$place), c(39,40), tolerance = .Machine$double.eps^0.4)
})
