#' Calculate grid mean layer matrix
#'
#' @param area An 'antares' area.
#' @param simulation_names Names of simulations to retrieve.
#' @param simulation_values Values for the simulation.
#' @param nb_cycle Number of times to run the algorithm.
#' @param district_name Name of the district used to store output.
#' @param max_mcyears Number of MC years to consider, by default all of them.
#' @param week_53 Water values for week 53, by default 0.
#' @param method Perform mean grid algorithm or grid mean algorithm ?
#' @param states_steps Steps to discrete steps levels between the reservoir
#'   capacity and zero. optimally assign the capacity of reservoir devised per
#'   nb_disc_stock used in pre simulations.
#' @param reservoir_capacity Reservoir capacity for the given area in GWh, if \code{NULL} (the default),
#'  value in Antares is used if available else a prompt ask the user the value to be used.
#' @param na_rm Remove NAs
#' @param correct_outliers If TRUE, outliers in Bellman values are replaced by spline
#'   interpolations. Defaults to FALSE.
#' @param opts
#'   List of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#'
#' @return a \code{data.table}
#' @export
#'
#' @importFrom antaresRead readInputTS
#' @importFrom utils txtProgressBar setTxtProgressBar
#'
#'
#'
meanGridLayer <- function(area, simulation_names, simulation_values = NULL, nb_cycle = 2L,
                          district_name = "water values district", max_mcyears = NULL,
                          week_53 = 0, method = c("mean-grid", "grid-mean"),
                          states_steps = 1000,
                          reservoir_capacity = NULL, na_rm = FALSE,
                          correct_outliers = FALSE,
                          opts = antaresRead::simOptions()) {





  assertthat::assert_that(class(opts) == "simOptions")

  # Number of weeks
  n_week <- 52

  # max hydro
  max_hydro <- get_max_hydro(area)

  if (is.null(simulation_values)) {
    simulation_values <- seq(from = 0, to = max_hydro, length.out = length(simulation_names))
    message(paste0("Using simulation_values: ", paste(simulation_values, collapse = ", ")))
  }

  if (is.null(max_mcyears)) {
    max_mcyears <- opts$parameters$general$nbyears
  }
  max_mcyears <- seq_len(max_mcyears)

  # Niveau max
  {if (is.null(reservoir_capacity)) {
    niveau_max <- get_reservoir_capacity(area = area)
    if (length(niveau_max) == 0) {
      ask_niveau_max <- "Failed to retrieve reservoir capacity from Antares, please specify value (in GWh, e.g. 10000 for France):\n"
      niveau_max <- readline(prompt = ask_niveau_max)
      niveau_max <- as.numeric(niveau_max)*1000
    }
    if (length(niveau_max) == 0)
      stop("Failed to retrieve reservoir capacity, please specify it explicitly with 'reservoir_capacity'.")
  } else {
    niveau_max <- reservoir_capacity
  }}

  # synchronizing between the simulations and the states
  #states_steps <- niveau_max/nb_sim

  # States matrix
  states <- matrix( rep(seq(from = niveau_max, to = 0, by = -states_steps), n_week + 1), byrow = FALSE, ncol = n_week + 1)

  if (length(week_53) == 1)
    week_53 <- rep_len(week_53, nrow(states))


  decision_space <- simulation_values

  decimals <- 6
  {
    tmp_name <- getSimulationNames(pattern = simulation_names[1], opts = opts)[1]
    tmp_opt <- setSimulationPath(path = opts$studyPath, simulation = tmp_name)
    inflow <- readAntares(areas = area, hydroStorage = TRUE, timeStep = "weekly", mcYears = "all", opts = tmp_opt)
    inflow <- inflow[order(mcYear, timeId)]
    inflow <- inflow[, list(area, tsId = mcYear, timeId, time, hydroStorage)]
    inflow[, timeId := gsub(pattern = "\\d{4}-w", replacement = "", x = time)]
    inflow[, timeId := as.numeric(timeId)]
    inflow <- inflow[, list(hydroStorage = sum(hydroStorage, na.rm = TRUE)), by = list(area, timeId, tsId)] # sum

  } # get the table (area,time,tsid,hydroStorage)

  options("antares" = opts)

  # Reward
  {reward <- get_Reward(simulation_names = simulation_names, district_name = district_name, opts = opts)
    reward <- reward[timeId %in% seq_len(n_week)]}

  # Reservoir (calque)
  {
    reservoir <- readReservoirLevels(area, timeStep = "weekly", byReservoirCapacity = FALSE, opts = opts)
    vars <- c("level_low", "level_avg", "level_high")
    reservoir[,
              (vars) := lapply(.SD, function(x) {round(x * max(states), decimals)}),
              .SDcols = vars
    ]   #nothing chnaged here !
  }

  # preparation donnees (generate a table of weeks and years)
  watervalues <- data.table(expand.grid(weeks = seq_len(n_week+1), years = max_mcyears))

  # add states
  {
    statesdt <- as.data.table(states)  #convert states matrix to data.table
    statesdt <- melt(data = statesdt, measure.vars = seq_len(ncol(states)), variable.name = "weeks", value.name = "states")
    statesdt[, weeks := as.numeric(gsub("V", "", weeks))] #turn weeks to numbers V1==> 1
    statesdt[, statesid := seq_along(states), by = weeks] # add id to refer to the state
    statesdt[, states := round(states, decimals)]
  }

  # add states plus 1
  {
    statesplus1 <- copy(statesdt)
    statesplus1[, weeks := weeks - 1]
    statesplus1 <- statesplus1[, list(states_next = list(unlist(states))), by = weeks]
    statesplus1 <- merge(x = statesdt, y = statesplus1, by = c("weeks"), all.x = TRUE)
    watervalues <- merge(x = watervalues, y = statesplus1, by = "weeks", all.x = TRUE, all.y = FALSE, allow.cartesian = TRUE)
  }

  # add inflow
  watervalues <- merge(x = watervalues, y = inflow[, list(weeks = timeId, years = tsId, hydroStorage)], by = c("weeks", "years"))
  #at this point water values is the table containing (weeks,year,states,statesid;states_next,hydroStorage)

  #add reward
  reward_l <- reward[, list(reward = list(unlist(.SD))), .SDcols = simulation_names, by = list(weeks = timeId, years = mcYear)]

  watervalues <- merge(x = watervalues, y = reward_l, by = c("weeks", "years"))
  #at this point we added the rewards for each weekly_amount

  # add reservoir
  watervalues <- merge(x = watervalues, y = reservoir[, list(weeks = timeId, level_low, level_high)], by = "weeks", all = TRUE)
        #here we added the lvl_high and low of the reservoir



  # Calcul by week
  next_week_values <- week_53

  max_hydro <- max(decision_space)

  watervalues[, value_node := NA_real_]

  verif_next_week <- list()

  {
    if (method == "mean-grid") {
      funGridMean <- mean
    } else {
      funGridMean <- max
    }
  } #mean or max method


{
states1 = watervalues$states
states_next = watervalues$states_next
value_reward = reward
value_inflow = watervalues$hydroStorage
level_high = watervalues$level_high
level_low = watervalues$level_low
value_node_next_week = next_week_values
E_max = max_hydro
} #calculate_value_node parameters



  for (n_cycl in seq_len(nb_cycle)) {

    cat("Calculating value nodes, cycle number:", n_cycl, "\n")

    pb <- txtProgressBar(min = 0, max = 51, style = 3)

    for (i in rev(seq_len(52))) { # rep(52:1, times = nb_cycle)
      watervalues[
        weeks == i,
        value_node := calculate_value_node(
          states = states, states_next = states_next, value_reward = reward,
          value_inflow = hydroStorage, decision_space = decision_space, level_high = level_high,
          level_low = level_low, value_node_next_week = next_week_values, niveau_max = niveau_max, E_max = max_hydro,
          method = method, na_rm = na_rm
        ),
        by = list(years, statesid)
      ]
      if (correct_outliers) {
        watervalues[weeks == i, value_node := correct_outliers(value_node), by = years]
      }

      next_week_values <- watervalues[weeks == i, list(vny = funGridMean(value_node, na.rm = TRUE)), by = statesid][, c(vny)]
      verif_next_week <- c(verif_next_week, list(next_week_values))

      setTxtProgressBar(pb = pb, value = 52 - i)

    }
    close(pb)
  }

    value_nodes_dt <- watervalues[, list(value_node = funGridMean(value_node, na.rm = TRUE)),
                                by = list(weeks, statesid)]


  # add states levels
  value_nodes_dt <- merge(x = value_nodes_dt, y = statesdt, by = c("weeks", "statesid"))

  # Calculate Usage values
  value_nodes_dt <- value_nodes_dt[order(weeks, -statesid)]
  value_nodes_dt[, value_node_dif := c(NA, diff(value_node)), by = weeks]
  value_nodes_dt[, states_dif := c(NA, diff(states)), by = weeks]
  value_nodes_dt[, vu := abs(value_node_dif / states_dif )]

  return(value_nodes_dt)

                                                            }
