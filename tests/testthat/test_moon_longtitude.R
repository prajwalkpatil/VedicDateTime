
library(testthat)
library(VedicDateTime)
source_test_helpers()

test_that("Check positive testcase", {
  skip_if(packageVersion("swephR") > "0.3.0", "New swephR version")
  expect_equal(moon_longitude(vd$jd), 346.5715, tolerance = .Machine$double.eps^0.4)
})
test_that("Check positive testcase", {
  skip_if(packageVersion("swephR") < "0.3.1", "Old swephR version")
  expect_equal(moon_longitude(vd$jd), 346.5713, tolerance = .Machine$double.eps^0.4)
})
