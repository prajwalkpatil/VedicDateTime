
library(testthat)
library(VedicDateTime)
source_test_helpers()

test_that("Check positive testcase", {
  skip_if(packageVersion("swephR") > "0.3.0", "New swephR version")
  expect_equal(yoga(vd$jd, vd$place), c(5,27,26,12), tolerance = .Machine$double.eps^0.4)
})
test_that("Check positive testcase", {
  skip_if(packageVersion("swephR") < "0.3.1", "Old swephR version")
  expect_equal(yoga(vd$jd, vd$place), c(5,27,26,14), tolerance = .Machine$double.eps^0.4)
})
