source("~/dev/VedicDateTime/tests/testthat/vedic_class.R")
library(testthat)
library(VedicDateTime)

masa_num <- masa(vd$jd, vd$place)

test_that("Check positive testcase", {
  expect_equal(samvatsara(vd$jd, masa_num), 36, tolerance = .Machine$double.eps^0.4)
})
