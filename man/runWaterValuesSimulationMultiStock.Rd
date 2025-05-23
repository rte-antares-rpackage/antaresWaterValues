% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/runWaterValuesSimulation.R
\name{runWaterValuesSimulationMultiStock}
\alias{runWaterValuesSimulationMultiStock}
\title{Run a simulation for calculating water values for a specific area}
\usage{
runWaterValuesSimulationMultiStock(
  list_areas,
  list_pumping,
  list_efficiency,
  simulation_name = "wv_sim_\%s",
  nb_mcyears = NULL,
  binding_constraint = "weekly_water_amount",
  path_solver = NULL,
  wait = TRUE,
  show_output_on_console = FALSE,
  overwrite = FALSE,
  opts = antaresRead::simOptions(),
  shiny = F,
  otp_dest = NULL,
  file_name = NULL,
  launch_simulations = T,
  reset_hydro = T,
  constraint_values = NULL,
  expansion = T,
  ...
)
}
\arguments{
\item{list_areas}{List of areas concerned by the simulation.}

\item{list_pumping}{List of bools to tell if pumping is available in areas}

\item{list_efficiency}{List of pumping efficiency}

\item{simulation_name}{character}

\item{nb_mcyears}{list}

\item{binding_constraint}{character}

\item{path_solver}{Character containing the Antares Solver path, argument passed to \code{\link[antaresEditObject]{runSimulation}}.}

\item{wait}{Argument passed to \code{\link[antaresEditObject]{runSimulation}}.}

\item{show_output_on_console}{Argument passed to \code{\link[antaresEditObject]{runSimulation}}.}

\item{overwrite}{If area or cluster already exists, should they be overwritten?}

\item{opts}{List of simulation parameters returned by the function
\code{antaresRead::setSimulationPath}}

\item{shiny}{Boolean. True to run the script in shiny mod.}

\item{otp_dest}{the path in which the script save Rdata file.}

\item{file_name}{the Rdata file name.}

\item{launch_simulations}{Boolean. True to to run the simulations.}

\item{reset_hydro}{Boolean. True to reset hydro inflow to 0 before the simulation.}

\item{constraint_values}{constraint values to use to run simulations, generated by the function \code{\link{constraint_generator}}}

\item{expansion}{Binary. True if mode expansion was used to run simulations}

\item{...}{further arguments passed to or from other methods.}
}
\description{
Run a simulation for calculating water values for a specific area
}
\note{
This function have side effects on the Antares study used, a fictive area is created and a new district as well.
}
