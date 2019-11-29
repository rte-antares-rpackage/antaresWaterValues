
# Setup study -------------------------------------------------------------


path <- tempdir()
# Unzip the study
setup_study(path, sourcedir)
# set simulation path in mode input
opts <- antaresRead::setSimulationPath(studyPath, "input")




# Tests -------------------------------------------------------------------

test_that("getReward() does not work when no simulation_names and no patterns", {
  expect_error(getReward(district_name = "a and b"), regexp = "not provided")
})
