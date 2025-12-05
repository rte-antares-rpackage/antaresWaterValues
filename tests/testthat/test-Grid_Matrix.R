test_that("Lower bound Grid_Matrix", {
  path0 <- tempdir()
  sourcedir <- system.file("extdata", package = "antaresWaterValues")
  studies <- list.files(
    path = sourcedir,
    pattern = "^antares-test-study.*\\.tar\\.gz$"
  )

  studies_names <- basename(studies)
  studies_names <- sub("\\.tar\\.gz$", "", studies_names)

  for (s in seq_along(studies)) {
    dir.create(file.path(path0, studies_names[s]))
    untar(file.path(sourcedir, studies[s]), exdir = file.path(path0, studies_names[s]))
  }

  opts <- antaresRead::setSimulationPath(file.path(path0, studies_names,"test_case"),"input")

  area <- "area"
  pumping <- T #T if pumping possible
  mcyears <- 1:3 # Monte Carlo years you want to use
  efficiency <- getPumpEfficiency(area,opts=opts)
  name = "3sim"

  load(paste0(opts$studyPath, "/user/", tolower(area),"_",name, ".RData"))

  reward_db <- get_Reward(
    simulation_names = simulation_res$simulation_names,
    simulation_values = simulation_res$simulation_values,
    opts=opts,
    area = area,
    mcyears = mcyears,
    efficiency = efficiency,
    method_old = T,
  )

  states_step_ratio = 1/21
  penalty_low = 0
  penalty_high = 0
  penalty_final_level = 0
  nb_cycle = 1

  lb = Grid_Matrix(
    area=area,
    reward_db = reward_db,
    mcyears = mcyears,
    states_step_ratio = states_step_ratio,
    opts = opts,
    efficiency=efficiency,
    penalty_low = penalty_low,
    penalty_high = penalty_high,
    penalty_final_level_low = penalty_final_level,
    penalty_final_level_high = penalty_final_level,
    nb_cycle = nb_cycle
  )$lower_bound
  expect_equal(lb, -44728328320)
})
