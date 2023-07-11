
library(testthat)
library(VedicDateTime)
source_test_helpers()

test_that("Check positive testcase", {
  expect_equal(tithi(vd$jd, vd$place), c(20,20,55,37), tolerance = .Machine$double.eps^0.2)
})
test_that("Check positive testcase", {
  expect_equal(tithi(vd$jd, vd$place), c(20,20,55,37), tolerance = .Machine$double.eps^0.2)
})
