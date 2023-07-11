
library(testthat)
library(VedicDateTime)
source_test_helpers()

test_that("Check positive testcase", {
  expect_equal(moon_longitude(vd$jd), 346.5715, tolerance = .Machine$double.eps^0.2)
})
test_that("Check positive testcase", {
  expect_equal(moon_longitude(vd$jd), 346.5713, tolerance = .Machine$double.eps^0.2)
})
