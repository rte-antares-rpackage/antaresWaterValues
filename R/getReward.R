#' Get the reward matrix from simulations
#'
#' @param simulation_names Names of the simulation to obtain the reward.
#' @param pattern A pattern to identify simulations.
#' @param district_name Name of the district used to store output.
#' @param opts
#'   List of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath} 
#'
#' @return a data.table
#' @export
#' 
#' @importFrom assertthat assert_that
#' @importFrom antaresRead setSimulationPath readAntares
#'
#' @examples
#' \dontrun{
#' 
#' # TODO
#' 
#' }
getReward <- function(simulation_names = NULL, pattern = NULL, district_name = "water values district", opts = antaresRead::simOptions()) {
  assertthat::assert_that(class(opts) == "simOptions")
  assertthat::assert_that(district_name %in% antaresRead::getDistricts())
  studyPath <- opts$studyPath
  if (is.null(simulation_names)) {
    if (is.null(pattern))
      stop("If 'simulation_names' is not provided, 'pattern' cannot be NULL.")
    simulation_names <- getSimulationNames(pattern = pattern, studyPath = studyPath)
  }
  opts_o <- lapply(
    X = simulation_names, 
    FUN = function(i) {
      suppressWarnings({
        antaresRead::setSimulationPath(path = studyPath, simulation = i)
      })
    }
  )
  water_used_list <- list()
  reward <- lapply(
    X = opts_o, 
    FUN = function(o) {
      res <- antaresRead::readAntares(districts = district_name, mcYears = "all", timeStep = "weekly", opts = o)
      # res <- res[, timeId := gsub(pattern = "\\d{4}-w", replacement = "", x = as.character(time))]
      res <- res[timeId == 53L, timeId := 1L]
      res <- res[, lapply(.SD, sum, na.rm = TRUE), by = list(timeId, mcYear), .SDcols = "OV. COST"]
      res$simulation <- o$name
      res
    }
  )
  # water_used_list <- lapply(
  #   X = opts_o,
  #   FUN = function(o) {
  #     wvini <- antaresEditObject::readIniFile(file = file.path(o$simPath, "watervalues.ini"))
  #     wvini$general$watervalue
  #   }
  # )
  # water_used_list <- stats::setNames(water_used_list, sapply(opts_o, `[[`, "name"))
  reward <- rbindlist(reward)
  reward <- dcast(reward, timeId + mcYear ~ simulation, value.var = "OV. COST")
  vars <- simulation_names # setdiff(names(reward), c("timeId", "mcYear"))
  setcolorder(x = reward, neworder = c("timeId", "mcYear", vars))
  reward <- reward[
    , (vars) := lapply(.SD, FUN = function(x) {
      get(vars[1]) - x
    }),
    .SDcols = vars
    ]
  
  # # Division valeur stock
  # reward <- reward[, (vars) := lapply(vars, function(x) {
  #   if (water_used_list[[x]] > 0) {
  #     get(x) / (water_used_list[[x]]*1e6)
  #   } else {
  #     get(x)
  #   }
  # })]
  
  # restore previous antares options
  options("antares" = opts)
  
  return(reward)
}
