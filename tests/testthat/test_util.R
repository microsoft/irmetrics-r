context("Internal utilities")

test_that("is.atomic.na is well-behaved", {
  expect_true(is.atomic.na(NA))
  expect_false(is.atomic.na(1))
  expect_false(is.atomic.na(c(NA,NA)))
  expect_false(is.atomic.na(c(1)))
})

test_that("prepend0 is well-behaved", {
  expect_equal(prepend0(1), c(0))
  expect_equal(prepend0(c(1)), c(0))
  expect_equal(prepend0(c(1,2)), c(0,1))
})
