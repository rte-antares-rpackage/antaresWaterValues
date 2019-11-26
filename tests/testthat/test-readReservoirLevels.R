
# Setup study -------------------------------------------------------------


path <- tempdir()
# Unzip the study
setup_study(path, sourcedir)
# set simulation path in mode input
opts <- antaresRead::setSimulationPath(studyPath, "input")




# Tests -------------------------------------------------------------------

test_that("readReservoirLevels() works for a v7 study", {
  expect_error(readReservoirLevels("z"), regexp = "Not a valid area")

  expect_equal(
    readReservoirLevels("a", timeStep = "daily"),
    data.table(
      level_low = rep(0, 365),
      level_avg = rep(0.5, 365),
      level_high = rep(1, 365),
      timeId = seq.Date(from = as.Date("2018-01-01"), by = "day", length.out = 365)
    )
  )
})
