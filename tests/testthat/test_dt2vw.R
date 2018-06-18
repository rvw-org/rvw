library(rvw)
context("basic tests for dt2vw")

test_that("dt2vw raises error when NAs in input dataset", {
  df <- data.frame(v1 = c(1,2,NA,4), v2 = letters[1:4], target = c(0,0,1,1))
  expect_error(rvw::dt2vw(data = df, fileName = "num_NA.vw", target = "target"),
               regexp = "Please remove any 'NA' values from data first.")
})
