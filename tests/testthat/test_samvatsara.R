
library(testthat)
library(VedicDateTime)
source_test_helpers()

masa_num <- masa(vd$jd, vd$place)

test_that("Check positive testcase", {
  expect_equal(samvatsara(vd$jd, masa_num), 36, tolerance = .Machine$double.eps^0.4)
})
