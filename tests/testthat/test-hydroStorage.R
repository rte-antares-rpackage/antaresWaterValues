
# Setup study -------------------------------------------------------------


path <- tempdir()
# Unzip the study
setup_study(path, sourcedir)
# set simulation path in mode input
opts <- antaresRead::setSimulationPath(studyPath, "input")




# Tests -------------------------------------------------------------------

test_that("Error when invalid area in resetHydroStorage()", {
  expect_error(resetHydroStorage("z"), regexp = "not a valid area")
})

test_that("resetHydroStorage() creates a backup file and sets mod.txt to zero", {
  resetHydroStorage("b")
  expect_true(file.exists(file.path(opts$inputPath, "hydro", "series", "b", "mod_backup.txt")))
  hydro_storage <- utils::read.table(file = file.path(opts$inputPath, "hydro", "series", "b", "mod.txt"))
  expect_equal(hydro_storage, data.frame(V1 = rep(0, 365)))
})
