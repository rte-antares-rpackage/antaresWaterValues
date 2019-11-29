
# Setup study -------------------------------------------------------------


path <- tempdir()
# Unzip the study
setup_study(path, sourcedir)
# set simulation path in mode input
opts <- antaresRead::setSimulationPath(studyPath, "input")




# Tests -------------------------------------------------------------------

test_that("getReservoirCapacity() works", {
  expect_equal(getReservoirCapacity("c"), 1234)
  expect_error(getReservoirCapacity("z"), regexp = "Not a valid area")
  expect_null(getReservoirCapacity("b"))
})
