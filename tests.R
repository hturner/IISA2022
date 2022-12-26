test_that("log_2 returns log to base 2", {
  expect_equal(log_2(2^3), 3)
  expect_equal(log_2(2^0), 0)
})

test_that("negative value throws error", {
  expect_error(log_2(-1))
})