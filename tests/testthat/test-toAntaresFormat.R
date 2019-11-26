
test_that("toAntaresFormat() creates a 365*101 numeric matrix", {
  m <- toAntaresFormat(fake_water_values)
  expect_equal(dim(toAntaresFormat(fake_water_values)), c(365, 101))
  expect_true(is.numeric(m))
})
