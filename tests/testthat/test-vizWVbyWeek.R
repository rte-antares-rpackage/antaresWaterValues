test_that("vizWVbyWeek() creates a ggplot", {
  expect_identical(
    class(vizWVbyWeek(fake_water_values, week = 20, add_band = "both")),
    c("gg", "ggplot")
  )
})
