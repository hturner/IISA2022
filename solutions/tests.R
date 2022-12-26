test_that("qq works correctly for standard normal distribution", {
  tol <- 1e-2
  expect_equal(qq(rnorm(100000)), list(slope = 1, intercept = 0),
               tol = tol)
  expect_equal(qq(rnorm(100000), sd = 1/2), list(slope = 2, intercept = 0),
               tol = tol)
  expect_equal(qq(rnorm(100000), mean = 2), list(slope = 1, intercept = -2),
               tol = tol)
})
