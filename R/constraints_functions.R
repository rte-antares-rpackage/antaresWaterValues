

#' This function disable binding constraints for \code{runWaterValuesSimulation}
#' @param constrain_value the value of the constraint
#' @param name_bc the name of the constraint.
#' @param opts
#'   List of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#' @importFrom antaresEditObject editBindingConstraint
#' @export

disable_constraint <- function(constraint_value,name_bc,opts){

  opts_i <- antaresEditObject::editBindingConstraint(name = name_bc, opts = opts,enabled = FALSE)

  if(constraint_value<0){
    mod <- "Pump"
  }else{ mod <- "Turb"}

  opts_i <- antaresEditObject::editBindingConstraint(name = mod, opts = opts,enabled = FALSE)

 return(opts_i)
}








#' This function generate binding constraints for \code{runWaterValuesSimulation}
#' @param constrain_value the value of the constraint
#' @param coeff the sens of the constraint notation in Antares.
#' @param name_bc the name of the constraint.
#' @param opts
#'   List of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#' @export


generate_constraints <- function(constraint_value,coeff,name_bc,opts){

  if(constraint_value<0){
    # Implement the flow sens in the study

    opts <- antaresEditObject::createBindingConstraint(
      name = "Pump",
      enabled = TRUE,
      operator = "greater",
      coefficients = -coeff,
      opts = opts,
      overwrite = TRUE,
      timeStep = "hourly"
    )

    # Implement binding constraint

    opts <- antaresEditObject::createBindingConstraint(
      name = name_bc,
      values = data.frame(equal = rep(constraint_value, times = 366)),
      enabled = TRUE,
      timeStep = "weekly",
      operator = "equal",
      overwrite = TRUE,
      coefficients = -coeff,
      opts = opts
    )

  }else{

    opts <-  antaresEditObject::createBindingConstraint(
      name = "Turb",
      enabled = TRUE,
      operator = "greater",
      coefficients = coeff,
      opts = opts,
      overwrite = TRUE,
      timeStep = "hourly"
    )

    opts <- antaresEditObject::createBindingConstraint(
      name = name_bc,
      values = data.frame(less = rep(constraint_value, times = 366)),
      enabled = TRUE,
      timeStep = "weekly",
      operator = "less",
      overwrite = TRUE,
      coefficients = coeff,
      opts = opts)
  }


}




#' Generate the list of constraint values of the link between the fictive area and the real one
#' @param area The area concerned by the simulation.
#' @param nb_disc_stock Number of simulation to launch, a vector of energy constraint.
#' @param pumping Boolean. True to take into account the pumping.
#' @param pumping_efficiency between 0 and 1. the pumping efficiency ratio.
#' @param opts
#'   List of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#' @export
constraint_generator <- function(area,nb_disc_stock,pumping=F,pumping_efficiency=0.75,opts)
{
  max_hydro <- get_max_hydro(area,opts)
  res_cap <- get_reservoir_capacity(area,opts)
  max_app <- max( antaresRead::readInputTS(hydroStorage = area , timeStep="weekly")$hydroStorage)
  maxi <- min(max_hydro$turb+max_app,res_cap)
  mini <- -max_hydro$pump*pumping_efficiency
  if(pumping){
    constraint_values <- seq(from = mini, to = maxi, length.out = nb_disc_stock)
    constraint_values <- round(constraint_values, 3)
    constraint_values[which(abs(constraint_values)==min(abs(constraint_values)))] <- 0
  }else{
    constraint_values <- seq(from = 0, to = maxi, length.out = nb_disc_stock)
    constraint_values <- round(constraint_values, 3)
  }

  return(constraint_values)
}
