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
#' @param constraint_value the value of the constraint
#' @param name_bc the name of the constraint.
#' @param pumping Boolean. True to take into account the pumping.
#' @param opts
#'   List of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#' @importFrom antaresEditObject editBindingConstraint
#' @export

disable_constraint <- function(constraint_value,name_bc,pumping=F,opts){

  opts <- antaresEditObject::editBindingConstraint(name = name_bc, opts = opts,enabled = FALSE)
  opts <- antaresEditObject::editBindingConstraint(name = "Turb", opts = opts,enabled = FALSE)
  if(pumping){
    opts <- antaresEditObject::editBindingConstraint(name = "Pump", opts = opts,enabled = FALSE)
  }
  return(opts)
}








#' This function generate binding constraints for \code{runWaterValuesSimulation}
#' @param constraint_value the value of the constraint
#' @param coeff the sens of the constraint notation in Antares.
#' @param name_constraint the name of the constraint.
#' @param efficiency in [0,1]. efficient ratio of pumping.
#' @param opts
#'   List of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#' @export


generate_constraints <- function(constraint_value,coeff,name_constraint,efficiency=0.75,opts){


  if(length(coeff)==2){

    opts <-  antaresEditObject::createBindingConstraint(
      name = "Turb",
      enabled = TRUE,
      operator = "greater",
      coefficients = coeff[2],
      opts = opts,
      overwrite = TRUE,
      timeStep = "hourly"
    )

    opts <- antaresEditObject::createBindingConstraint(
      name = name_constraint,
      values = data.frame(less = rep(constraint_value, times = 366)),
      enabled = TRUE,
      timeStep = "weekly",
      operator = "less",
      overwrite = TRUE,
      coefficients = coeff[1],
      opts = opts)
  }else{


    # Implement the flow sens in the study Pumping

    opts <- antaresEditObject::createBindingConstraint(
      name = "Pump",
      enabled = TRUE,
      operator = "greater",
      coefficients = -coeff[3],
      opts = opts,
      overwrite = TRUE,
      timeStep = "hourly"
    )

    # Implement the flow sens in the study Turbining

    opts <-  antaresEditObject::createBindingConstraint(
      name = "Turb",
      enabled = TRUE,
      operator = "greater",
      coefficients = coeff[2],
      opts = opts,
      overwrite = TRUE,
      timeStep = "hourly"
    )



    # Implement binding constraint

    opts <- antaresEditObject::createBindingConstraint(
      name = name_constraint,
      values = data.frame(equal = rep(constraint_value, times = 366)),
      enabled = TRUE,
      timeStep = "weekly",
      operator = "equal",
      overwrite = TRUE,
      coefficients = c(coeff[1],efficiency*coeff[3]),
      opts = opts)
  }


  return(opts)
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
    assertthat::assert_that(nb_disc_stock>=3)
    total <- maxi-mini

    pump_rat <- 2
    turb_rat <- 2
    if (nb_disc_stock>=4){
      for (i in 1:(nb_disc_stock+1-4)){
        inc_pump <- abs(abs(mini)/(pump_rat+1)-abs(maxi)/turb_rat)
        inc_turb <- abs(abs(maxi)/(turb_rat+1)-abs(mini)/pump_rat)
        if (inc_pump<inc_turb){
          pump_rat <- pump_rat+1
        } else {
          turb_rat <- turb_rat+1
        }
      }
    }
    constraint_values_pump <- seq(from=mini,to=0,length.out=pump_rat)
    constraint_values_turb <- seq(from=0,to=maxi,length.out=turb_rat)

    constraint_values <- append(constraint_values_pump,constraint_values_turb)
    constraint_values <- constraint_values[!duplicated(constraint_values)]

    constraint_values <- round(constraint_values)

  }else{
    assertthat::assert_that(nb_disc_stock>=2)
    constraint_values <- seq(from = 0, to = maxi, length.out = nb_disc_stock)
    constraint_values <- round(constraint_values)
  }

  return(constraint_values)
}




generate_link_coeff <- function(area,fictive_area, pumping = FALSE, opts = simOptions()){

  # For the case of the pumping node, the constraint will be applied on the flow from the real area to the pumping node
  if(pumping == TRUE & grepl("_pump$", fictive_area)){
    if (match(area, sort(c(area, fictive_area))) == 1) {
      coeff <- stats::setNames(-1, paste(area, fictive_area, sep = "%"))
    } else {
      coeff <- stats::setNames(1, paste(fictive_area, area, sep = "%"))
    }
  } #Otherwise, the constraint will be applied on the generation from the thermal cluster
  else{
    cluster_desc <- readClusterDesc(opts)
    fictive_cluster <- cluster_desc[area == fictive_area, cluster]
    coeff1 <- stats::setNames(1, paste(fictive_area, fictive_cluster, sep = "."))

    if (match(area, sort(c(area, fictive_area))) == 1) {
      coeff2 <- stats::setNames(-1, paste(area, fictive_area, sep = "%"))
    } else {
      coeff2 <- stats::setNames(1, paste(fictive_area, area, sep = "%"))
    }
    coeff <- c(coeff1, coeff2)
  }

  return(coeff)
}
