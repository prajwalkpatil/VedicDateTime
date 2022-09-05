source("~/dev/VedicDateTime/tests/testthat/vedic_class.R")
library(testthat)
library(VedicDateTime)

test_that("Check positive testcase", {
  expect_equal(karana(vd$jd, vd$place), c(39,40), tolerance = .Machine$double.eps^0.4)
})
