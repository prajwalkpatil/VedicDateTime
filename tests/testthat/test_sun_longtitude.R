
library(testthat)
library(VedicDateTime)
source_test_helpers()

test_that("Check positive testcase", {
  expect_equal(sun_longitude(vd$jd), 114.9058, tolerance = .Machine$double.eps^0.4)
})
