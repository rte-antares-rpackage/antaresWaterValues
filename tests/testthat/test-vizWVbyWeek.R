test_that("vizWVbyWeek() creates a ggplot", {
  expect_identical(
    class(vizWVbyWeek(fake_water_values, week = 20, add_band = "both")),
    c("gg", "ggplot")
  )
})

test_that("vizWVbyWeek() creates a ggplot when add_band = 'spline'", {
  expect_identical(
    class(vizWVbyWeek(fake_water_values, week = 20, add_band = "spline")),
    c("gg", "ggplot")
  )
})

test_that("vizWVbyWeek() creates a ggplot when add_band = 'linear'", {
  expect_identical(
    class(vizWVbyWeek(fake_water_values, week = 20, add_band = "linear")),
    c("gg", "ggplot")
  )
})
