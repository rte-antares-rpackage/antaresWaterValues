
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

test_that("resetHydroStorage() creates a hydro storage file when needed", {
  storage_file <- file.path(opts$inputPath, "hydro", "series", "c", "mod.txt")
  file.remove(storage_file)
  expect_message(resetHydroStorage("c"), regexp = "No hydro storage series for this area, creating one")
  file.exists(storage_file)
})


test_that("Error when invalid area in restoreHydroStorage()", {
  expect_error(restoreHydroStorage("z"), regexp = "not a valid area")
})

test_that("restoreHydroStorage() prints an appropriate message when there is no backup file", {
  expect_message(restoreHydroStorage("a"), regexp = "No backup found")
})

test_that("restoreHydroStorage() replaces the storage file by the backup file and removes the backup file", {
  backup <- readLines(file.path(opts$inputPath, "hydro", "series", "b", "mod_backup.txt"))
  restoreHydroStorage("b")
  new_storage_file <- file.path(opts$inputPath, "hydro", "series", "b", "mod.txt")
  expect_identical(backup, readLines(new_storage_file))
})
