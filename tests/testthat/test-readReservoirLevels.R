
# Setup study v7 -------------------------------------------------------------


path <- tempdir()
# Unzip the study
setup_study(path, sourcedir)
# set simulation path in mode input
opts <- antaresRead::setSimulationPath(studyPath, "input")




# Tests v7 -------------------------------------------------------------------

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
    readReservoirLevels("c", timeStep = "monthly", byReservoirCapacity = FALSE),
    data.table(
      timeId = as.character(seq.Date(as.Date("2018-01-01"), by = "month", length.out = 12)),
      level_low = rep(0, 12),
      level_avg = rep(0.5, 12),
      level_high = rep(1, 12)
    )
  )
})



# Setup study v6 -------------------------------------------------------------


path <- tempdir()
# Unzip the study
setup_study(path, sourcedir, version = "v6")
# set simulation path in mode input
opts <- antaresRead::setSimulationPath(studyPath, "input")



# Tests v6 ----------------------------------------------------------------

test_that("readReservoirLevels() works for a v6 study", {
  expect_error(readReservoirLevels("z"), regexp = "Not a valid area")
  
  expect_equal(
    readReservoirLevels("c", timeStep = "monthly", byReservoirCapacity = FALSE),
    data.table(
      date = seq.Date(as.Date("2018-01-01"), by = "month", length.out = 12),
      level_low = rep(0, 12),
      level_avg = rep(0.5, 12),
      level_high = rep(1, 12)
    )
  )
})


