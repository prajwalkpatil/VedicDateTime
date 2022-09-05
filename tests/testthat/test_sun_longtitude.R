source("~/dev/VedicDateTime/tests/testthat/vedic_class.R")
library(testthat)
library(VedicDateTime)

test_that("Check positive testcase", {
  expect_equal(sun_longitude(vd$jd), 114.9058, tolerance = .Machine$double.eps^0.4)
})
