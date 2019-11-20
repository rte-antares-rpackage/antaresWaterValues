test_that("correct_outliers() works", {
  expect_equal(
    correct_outliers(c(-Inf, 2, 4, NA, 3, 5, 10000)),
                     c(-Inf, 2, 4, NA, 3, 5, 16)
  )
})
