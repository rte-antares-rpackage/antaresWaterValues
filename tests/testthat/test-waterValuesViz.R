test_that("waterValuesViz() creates a ggplot", {
  expect_identical(
    class(waterValuesViz(fake_water_values)),
    c("gg", "ggplot")
  )
})

test_that("waterValuesViz() creates a ggplot when add_band=TRUE", {
  expect_identical(
    class(waterValuesViz(fake_water_values, add_band = TRUE)),
    c("gg", "ggplot")
  )
})
