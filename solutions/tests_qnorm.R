test_that("qq_norm works as expected", {
  tol <- 0.0001
  expect_equal(qq_norm(rnorm(100000)), 
               list(slope = 1, intercept = 0),
               tol = tol)
})