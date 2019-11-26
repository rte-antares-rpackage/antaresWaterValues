
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
    readReservoirLevels("c", timeStep = "daily", byReservoirCapacity = FALSE),
    data.table(
      level_low = rep(0, 365),
      level_avg = rep(0.5, 365),
      level_high = rep(1, 365),
      timeId = seq.Date(from = as.Date("2018-01-01"), by = "day", length.out = 365)
    )
  )
  
  expect_equal(
    readReservoirLevels("c", timeStep = "daily", byReservoirCapacity = TRUE),
    data.table(
      level_low = rep(0, 365)*1234/1e6,
      level_avg = rep(0.5, 365)*1234/1e6,
      level_high = rep(1, 365)*1234/1e6,
      timeId = seq.Date(from = as.Date("2018-01-01"), by = "day", length.out = 365)
    )
  )
  
  expect_equal(
    readReservoirLevels("c", timeStep = "weekly", byReservoirCapacity = FALSE),
    data.table(
      timeId = 1:52,
      level_low = rep(0, 52),
      level_avg = rep(0.5, 52),
      level_high = rep(1, 52)
    )
  )
  
  expect_equal(
    a <<- readReservoirLevels("c", timeStep = "monthly", byReservoirCapacity = FALSE),
    b <<- data.table(
      timeId = as.character(seq.Date(as.Date("2018-01-01"), by = "month", length.out = 12)),
      level_low = rep(0, 12),
      level_avg = rep(0.5, 12),
      level_high = rep(1, 12)
    )
  )
})
