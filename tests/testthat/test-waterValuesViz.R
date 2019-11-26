test_that("waterValuesViz() creates a ggplot", {
  expect_identical(
    class(waterValuesViz(fake_water_values)),
    c("gg", "ggplot")
  )
})
