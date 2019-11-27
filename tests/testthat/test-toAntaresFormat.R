
test_that("toAntaresFormat() creates a 365*101 numeric matrix", {
  m <- toAntaresFormat(fake_water_values)
  expect_equal(dim(toAntaresFormat(fake_water_values)), c(365, 101))
  expect_true(is.numeric(m))
})

test_that("toAntaresFormat() works when water value is a constant", {
  wv <- data.table::data.table(
    weeks = rep(1:52, each = 357),
    statesid = rep(357:1, 52),
    states = seq(0, length.out = 357),
    vu = rep(80, 52*357)
  )
  
  mat <- toAntaresFormat(wv)
  dimnames(mat) <- NULL
  
  expect_equal(
    mat,
    matrix(rep(80, 365*101), nrow = 365)
  )
})
