context("Foraging metrics")

expect_01 <- function(x) {
  expect_true(all(x >= 0))
  expect_true(all(x <= 1))
}

sanity <- function(m) {
  expect_is(m, "irmetric")
  expect_01(m$metric)
  expect_01(m$C)
  expect_01(m$C1)
  expect_01(m$C2)
  expect_01(m$W)
  expect_01(m$L)
  expect_true(all(m$cum.metric <= m$metric))
}

test_that("IFT is well-behaved", {
  tmp <- IFT(c(1), c(1), rationality=99, target=1, A=0, intercept1=1)
  sanity(tmp) ; expect_equal(tmp$metric, 1)
  tmp <- IFT(c(0), c(1), rationality=99, target=1, A=0, intercept1=1)
  sanity(tmp) ; expect_equal(tmp$metric, 0)
})

test_that("IFT complains with bad parameters", {
  expect_error(IFT(1, 1, rationality=999, target=1, A=0), "Need either b1 or intercept1")
})

test_that("IFT can look like RR", {
  tmp <- IFT(c(0), c(1), target=1, A=0, rationality=999, intercept1=.999)
  sanity(tmp) ; expect_equal(tmp$metric, 0, tolerance=0.001)
  tmp <- IFT(c(1), c(1), target=1, A=0, rationality=999, intercept1=.999)
  sanity(tmp) ; expect_equal(tmp$metric, 1, tolerance=0.001)
  tmp <- IFT(c(0,0,0,1), c(1,1,1,1), target=1, A=0, rationality=999, intercept1=.999)
  sanity(tmp) ; expect_equal(tmp$metric, .25, tolerance=0.001)
  tmp <- IFT(c(0,0,0,1,1), c(1,1,1,1,1), target=1, A=0, rationality=999, intercept1=.999)
  sanity(tmp) ; expect_equal(tmp$metric, .25, tolerance=0.001)
})

test_that("IFT can look sort of like RR with higher target", {
  tmp <- IFT(c(0), c(1), target=2, A=0, rationality=999, intercept1=.999)
  sanity(tmp) ; expect_equal(tmp$metric, 0, tolerance=0.001)
  tmp <- IFT(c(1,0,0,0,1), c(1,1,1,1,1), target=1, A=0, rationality=999, intercept1=.999)
  sanity(tmp) ; expect_equal(tmp$metric, 1, tolerance=0.001)
  tmp <- IFT(c(0,0,0,1,1), c(1,1,1,1,1), target=1, A=0, rationality=999, intercept1=.999)
  sanity(tmp) ; expect_equal(tmp$metric, .25, tolerance=0.001)
})

test_that("IFT can look like RBP", {
  # note that C=int-int^2; we can mimic p up to .5
  intercept.to.use <- function(p) { .5 * (sqrt(1-4*p)+1) }
  tmp <- IFT(c(1,0,0,0,0), c(1,1,1,1,1), target=999, A=0, rationality=0, intercept1=intercept.to.use(.2))
  sanity(tmp) ; expect_equal(tmp$metric, .8*.2^0, tolerance=0.001)
  expect_equal(tmp$C, rep(.2, 5))
  tmp <- IFT(c(0,0,0,0,1), c(1,1,1,1,1), target=999, A=0, rationality=0, intercept1=intercept.to.use(.2))
  sanity(tmp) ; expect_equal(tmp$metric, .8*.2^4, tolerance=0.001)
  expect_equal(tmp$C, rep(.2, 5))
})

test_that("IFT matches examples in SIGIR paper", {
  # these are C2 only, so we hold C1 constant with target=0, intercept1=1
  tmp <- IFT(c(.5), c(1), target=0, A=0.5, rationality=1, intercept1=1, b2=1)
  sanity(tmp) ; expect_equal(tmp$C2[[1]], .5)

  tmp <- IFT(c(0), c(1), target=0, A=0.5, rationality=100, intercept1=1, b2=1)
  sanity(tmp) ; expect_equal(tmp$C2[[1]], 0)
  tmp <- IFT(c(.4), c(1), target=0, A=0.5, rationality=100, intercept1=1, b2=1)
  sanity(tmp) ; expect_equal(tmp$C2[[1]], 0, tolerance=0.001)
  tmp <- IFT(c(.5), c(1), target=0, A=0.5, rationality=100, intercept1=1, b2=1)
  sanity(tmp) ; expect_equal(tmp$C2[[1]], .5)
  tmp <- IFT(c(.6), c(1), target=0, A=0.5, rationality=100, intercept1=1, b2=1)
  sanity(tmp) ; expect_equal(tmp$C2[[1]], 1, tolerance=0.001)
  tmp <- IFT(c(1), c(1), target=0, A=0.5, rationality=100, intercept1=1, b2=1)
  sanity(tmp) ; expect_equal(tmp$C2[[1]], 1)
})
