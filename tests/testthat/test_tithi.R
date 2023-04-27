
library(testthat)
library(VedicDateTime)
source_test_helpers()

test_that("Check positive testcase", {
  skip_if(packageVersion("swephR") > "0.3.0", "New swephR version")
  expect_equal(tithi(vd$jd, vd$place), c(20,20,55,35), tolerance = .Machine$double.eps^0.4)
})
test_that("Check positive testcase", {
  skip_if(packageVersion("swephR") < "0.3.1", "Old swephR version")
  expect_equal(tithi(vd$jd, vd$place), c(20,20,55,37), tolerance = .Machine$double.eps^0.4)
})
