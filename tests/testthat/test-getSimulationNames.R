
# Setup study -------------------------------------------------------------


path <- tempdir()
# Unzip the study
setup_study(path, sourcedir)
# set simulation path in mode input
opts <- antaresRead::setSimulationPath(studyPath, "input")




# Tests -------------------------------------------------------------------


test_that("getSimulationNames() works", {
  expect_equal(length(getSimulationNames(pattern = "xyz")), 0)
})
