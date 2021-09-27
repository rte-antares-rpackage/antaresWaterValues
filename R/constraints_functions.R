#' simulations names and values generator
#' @param area The area concerned by the simulation.
#' @param simulation_name The used name of the simulation.
#' @param nb_disc_stock Number of simulation launched.
#' @param pumping Boolean. True to take into account the pumping.
#' @param opts
#'   List of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#'
#' @note This function have no side effects on the Antares study used.
#' It's used in case you forget to save the output of script \code{runWaterValuesSimulation} to generate the output.
#' The results are meaningless unless you already launched the \code{runWaterValuesSimulation} with the same parameters.
#' @export
simulation_names_values <- function(area,simulation_name,nb_disc_stock,
                                    pumping=F,opts = antaresRead::simOptions()){


  if(!endsWith(simulation_name,"%s")){
    simulation_name <- paste0(simulation_name,"%s")
  }

  constraint_values <- constraint_generator(area,nb_disc_stock,pumping,
                                            pumping_efficiency=1,opts)

  simulation_names <- vector(mode = "character", length = length(constraint_values))

  simulation_names <- lapply(constraint_values,FUN = naming <- function(x) {
    sprintf(simulation_name, format(x, decimal.mark = ","))
  })

  simulation_names <- unlist(simulation_names)

  simulation_res <- list(
    simulation_names = simulation_names,
    simulation_values = constraint_values
  )

  return(simulation_res)
}






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
      values = data.frame(equal = rep(-constraint_value, times = 366)),
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
constraint_generator <- function(area,nb_disc_stock,pumping=F,pumping_efficiency=NULL,opts)
{
  if(is.null(pumping_efficiency))
  { pumping_efficiency <- getPumpEfficiency(area,opts=opts)}


  max_hydro <- get_max_hydro(area,opts)
  res_cap <- get_reservoir_capacity(area,opts)
  max_app <- max( antaresRead::readInputTS(hydroStorage = area , timeStep="weekly")$hydroStorage)
  maxi <- min(max_hydro$turb,res_cap+max_app)
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
