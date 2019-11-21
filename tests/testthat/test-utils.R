test_that("correct_outliers() works", {
  expect_equal(
    correct_outliers(c(-Inf, 2, 4, NA, 3, 5, 10000)),
                     c(-Inf, 2, 4, NA, 3, 5, 16)
  )
})

test_that("remove_outliers() works", {
  expect_equal(
    remove_outliers(c(1:4, 1000)),
                    c(1:4, NA)
  )
})

test_that("%||% works", {
  expect_equal(3 %||% 5, 3)
  expect_equal(NULL %||% 5, 5)
})

test_that("num_equal() works", {
  expect_true(num_equal(1/2 - 1/3, 1/6))
})

test_that("hasName() works", {
  x <- list(abc = 1, def = 2)
  expect_true(hasName(x, "abc"))
  expect_false(hasName(x, "a"))
})

test_that("mean_finite() works", {
  expect_identical(mean_finite(c(Inf, -Inf, Inf, NA)), -Inf)
  expect_equal(mean_finite(c(Inf, 1:3, NA)), 2)
})

test_that("expand_to_days() works", {
  expect_equal(expand_to_days(rep(1000, 52)), rep(1000, 365))
  expect_equal(
    expand_to_days(c(rep(NA, 20), 2000, 2000, rep(Inf, 30))),
    c(rep(0, 20*7), rep(2000, 2*7), rep(0, 30*7 + 1))
  )
})
