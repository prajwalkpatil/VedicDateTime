
library(testthat)
library(VedicDateTime)
source_test_helpers()

masa_num <- masa(vd$jd, vd$place)

test_that("Check positive testcase", {
  expect_equal(ritu(masa_num), c(2, 0), tolerance = .Machine$double.eps^0.4)
})
