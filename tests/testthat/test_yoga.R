source("~/dev/VedicDateTime/tests/testthat/vedic_class.R")
library(testthat)
library(VedicDateTime)

test_that("Check positive testcase", {
  expect_equal(yoga(vd$jd, vd$place), c(5,27,26,12), tolerance = .Machine$double.eps^0.4)
})
