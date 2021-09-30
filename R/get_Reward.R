#' Get the reward matrix from simulations
#'
#' @param simulation_names Names of the simulation to obtain the reward.
#' @param pattern A pattern to identify simulations.
#' @param district_name Name of the district used to store output.
#' @param opts
#'   List of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#'
#' @return a data.table {timeid,MCyear,simulation overall cost}
#' @export
#' @import data.table
#' @importFrom assertthat assert_that
#' @importFrom antaresRead setSimulationPath readAntares getDistricts
#'



get_Reward <- function(simulation_names = NULL, pattern = NULL,
                       district_name = "water values district",
                       opts = antaresRead::simOptions()) {

  assertthat::assert_that(class(opts) == "simOptions")
  assertthat::assert_that(district_name %in% antaresRead::getDistricts(opts=opts))
  studyPath <- opts$studyPath

  # just a test if there is a simulation done or not
  {if (is.null(simulation_names)) {
    if (is.null(pattern))
      stop("If 'simulation_names' is not provided, 'pattern' cannot be NULL.")
    simulation_names <- getSimulationNames(pattern = pattern, studyPath = studyPath)
  }}

  # this part prepare the environment of each simulation
  {
    opts_o <- lapply(
      X = simulation_names,
      FUN = function(i) {
        suppressWarnings({
          antaresRead::setSimulationPath(path = studyPath, simulation = i)
        })
      }
    )
  }


  #generate a table containing the year, the time id and OVerall cost
  {reward <- lapply(
    X = opts_o,
    FUN = function(o) {
      res <- antaresRead::readAntares(districts = district_name, mcYears = "all", timeStep = "weekly", opts = o)
      res <- res[timeId == 53L, timeId := 1L]
      res <- res[, lapply(.SD, sum, na.rm = TRUE), by = list(timeId, mcYear), .SDcols = "OV. COST"]
      res$simulation <- o$name
      res
    }
  )}


  reward <- rbindlist(reward)   #merge the all simulations tables together
  reward <- dcast(reward, timeId + mcYear ~ simulation, value.var = "OV. COST")
  vars <- simulation_names # setdiff(names(reward), c("timeId", "mcYear"))
  setcolorder(x = reward, neworder = c("timeId", "mcYear", vars))


  ind <- which(endsWith(vars,"_0"))

  reward <- reward[
    , (vars) := lapply(.SD, FUN = function(x) {
      get(vars[ind]) - x
    }),
    .SDcols = vars
  ]

  options("antares" = opts)
  return(reward)
}
