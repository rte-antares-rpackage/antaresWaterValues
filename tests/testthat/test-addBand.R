
# Setup study -------------------------------------------------------------


path <- tempdir()
# Unzip the study
setup_study(path, sourcedir)
# set simulation path in mode input
opts <- antaresRead::setSimulationPath(studyPath, "input")




# Tests -------------------------------------------------------------------


test_that("addBandVU() works", {
  expect_error(addBandVU(fake_water_values), regexp = "must provide an area")
  expect_s3_class(addBandVU(fake_water_values, area = "a"), "data.frame")
})

test_that("getUnserverdEnergyCost() throws an error when area does not exist", {
  expect_error(getUnserverdEnergyCost("z"), regexp = "Not a valid area")
})
