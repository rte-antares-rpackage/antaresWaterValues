# Copy the test study in a temporary folder

path <- tempdir()
sourcedir <- system.file("testdata", package = "antaresWaterValues")

# Hack: For some unknown reason, this script is executed at some point of
# the R CMD CHECK before package is correctly installed and tests actually run. 
# The following "if" prevents errors at this step

setup_study <- function(path, sourcedir, version = "v7") {
  if (sourcedir != "") {
    if (version == "v7") {
      untar(file.path(sourcedir, "antares-test-study-v7.tar.gz"), exdir = path)
    } else if (version == "v6") {
      untar(file.path(sourcedir, "antares-test-study.tar.gz"), exdir = path)
    }
    
    assign("studyPath", file.path(path, "test_case"), envir = globalenv())
    assign("nweeks", 2, envir = globalenv())
  }
}

fake_water_values <- data.table::data.table(
  weeks = rep(1:52, each = 201),
  statesid = rep(201:1, 52),
  states = seq(0, 10, by = 0.05),
  vu = runif(n = 52*201, min = 0, max = 150)
)
fake_water_values[states > 7.5 | states < 4, vu := NaN]
