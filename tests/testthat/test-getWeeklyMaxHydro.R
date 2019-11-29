
# Setup study v7 -------------------------------------------------------------


path <- tempdir()
# Unzip the study
setup_study(path, sourcedir)
# set simulation path in mode input
opts <- antaresRead::setSimulationPath(studyPath, "input")




# Tests v7 -------------------------------------------------------------------

test_that("getWeeklyMaxHydro() works for a v7 study", {
  expect_equal(getWeeklyMaxHydro("c"), 0.0504)
})



# Setup study v6 -------------------------------------------------------------


path <- tempdir()
# Unzip the study
setup_study(path, sourcedir, version = "v6")
# set simulation path in mode input
opts <- antaresRead::setSimulationPath(studyPath, "input")



# Tests v6 ----------------------------------------------------------------

test_that("getWeeklyMaxHydro() works for a v6 study", {
  expect_equal(getWeeklyMaxHydro("c"), 0.0504)
})


