#' This function disable binding constraints for \code{runWaterValuesSimulation}
#'
#' @param name_bc the name of the constraint.
#' @param opts
#'   List of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#' @param pumping Boolean. True to take into account the pumping.
#' @param area Area used to calculate watervalues
#'

disable_constraint <- function(name_bc,opts,pumping=F,area=NULL){

  suppressWarnings({bc <- antaresRead::readBindingConstraints(opts)})
  if (!is.null(bc)){
    if (name_bc %in% names(bc)){
      opts <- antaresEditObject::removeBindingConstraint(name = name_bc, opts = opts)
      opts <- antaresEditObject::removeBindingConstraint(name = paste0("turb_",area), opts = opts)
      if(pumping){
        opts <- antaresEditObject::removeBindingConstraint(name = paste0("pump_",area), opts = opts)
      }
    }
  }

  return(opts)
}

#' This function generate binding constraints for \code{runWaterValuesSimulation}
#'
#' @param coeff the sens of the constraint notation in Antares.
#' @param name_constraint the name of the constraint.
#' @param efficiency in [0,1]. efficient ratio of pumping.
#' @param opts
#'   List of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#' @param area Area used to calculate watervalues


generate_constraints <- function(coeff,name_constraint,efficiency=0.75,opts,area=NULL){

  if (opts$antaresVersion<870){
    default_values = matrix(data = rep(0, 8760 * 3), ncol = 3)
    default_values_weekly = matrix(data = rep(0, 365 * 3), ncol = 3)
  } else {
    df <- matrix(data = rep(0, 8760), ncol = 1)
    default_values = list(gt= df)
    default_values_weekly = list(eq = matrix(data = rep(0, 365), ncol = 1))
  }
  if(length(coeff)==1){

    opts <-  antaresEditObject::createBindingConstraint(
      name =  paste0("turb_",area),
      enabled = TRUE,
      operator = "greater",
      coefficients = coeff[1],
      opts = opts,
      overwrite = TRUE,
      timeStep = "hourly",
      values = default_values
    )

    opts <- antaresEditObject::createBindingConstraint(
      name = name_constraint,
      enabled = TRUE,
      timeStep = "weekly",
      operator = "equal",
      overwrite = TRUE,
      coefficients = coeff[1],
      values = default_values_weekly,
      group = "watervalues",
      opts = opts)
  }else{

    # Implement the flow sens in the study Pumping

    opts <- antaresEditObject::createBindingConstraint(
      name = paste0("pump_",area),
      enabled = TRUE,
      operator = "greater",
      coefficients = -coeff[2],
      opts = opts,
      overwrite = TRUE,
      timeStep = "hourly",
      values = default_values
    )

    # Implement the flow sens in the study Turbining

    opts <-  antaresEditObject::createBindingConstraint(
      name = paste0("turb_",area),
      enabled = TRUE,
      operator = "greater",
      coefficients = coeff[1],
      opts = opts,
      overwrite = TRUE,
      timeStep = "hourly",
      values = default_values
    )



    # Implement binding constraint
    opts <- antaresEditObject::createBindingConstraint(
      name = name_constraint,
      enabled = TRUE,
      timeStep = "weekly",
      operator = "equal",
      overwrite = TRUE,
      coefficients = c(coeff[1],coeff[2]*efficiency),
      values = default_values_weekly,
      group = "watervalues",
      opts = opts)


  }

  return(opts)
}

#' Modify time-series of clusters in fictive_area_bc to implement the constraint value
#' for each week and each MC year
#'
#' @param constraint_value Data.frame {week,sim,u}
#' @param name_constraint the name of the constraint.
#' @param opts List of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}

generate_rhs_bc <- function(constraint_value,name_constraint,opts){

  if ("mcYear" %in% names(constraint_value)){
    scenarios <- unique(constraint_value$mcYear)
    constraint_value <- constraint_value %>%
      dplyr::arrange(.data$mcYear) %>%
      tidyr::pivot_wider(names_from = "mcYear",values_from = "u")
  } else {
    scenarios <- NULL
  }
  constraint_value <- constraint_value %>%
    dplyr::arrange(.data$week) %>%
    dplyr::select(-c("sim","week"))
  constraint_value <- as.matrix(constraint_value)
  constraint_value = rbind(matrix(rep(constraint_value/7, each=7),ncol=ncol(constraint_value)),
                          matrix(rep(0,2*ncol(constraint_value)),ncol=ncol(constraint_value)))

  if (opts$antaresVersion<870){
    values = data.frame(equal = constraint_value)
  } else {
    values = list(eq= constraint_value)
    }
  antaresEditObject::editBindingConstraint(
    name = name_constraint,
    operator = "equal",
    values = values,
    opts = opts
  )

  if (!is.null(scenarios)){

    names_sb <- c()
    values_sb <- c()

    for (i in 1:length(scenarios)){
      names_sb <- c(names_sb,paste("bc","watervalues",
                                   scenarios[i]-1,sep=","))
      values_sb <- c(values_sb,as.integer(i))
    }

    new_sb <- unlist(list(values_sb))
    names(new_sb) <- names_sb

    sb_file <- antaresRead::readIniFile(file.path(opts$studyPath, "settings", "scenariobuilder.dat"))

    if (length(names(sb_file))>0){
      assertthat::assert_that(length(names(sb_file))==1,
                              msg="There should be only one ruleset in scenario builder.")

      name_ruleset <- names(sb_file)[[1]]
      sb_file[[name_ruleset]] <- append(sb_file[[name_ruleset]],new_sb)

    } else {
      sb_file = new_sb
      names(sb_file) = c("Default ruleset")
    }

    antaresEditObject::writeIni(listData = sb_file,
                                pathIni = file.path(opts$studyPath, "settings", "scenariobuilder.dat"),
                                overwrite = TRUE, default_ext = ".dat")

    Sys.sleep(1)
  }


  return(opts)

}

#' Generate the list of constraint values of the link between the fictive area and the real one
#' Used to run simulations in \code{runWaterValuesSimulation} and also to estimate reward functions
#' in functions such as \code{Grid_Matrix} and \code{iterations_simulation_DP}
#'
#' @param area The area concerned by the simulation.
#' @param nb_disc_stock Number of constraint values wanted for each week
#' @param pumping Boolean. True to take into account the pumping.
#' @param pumping_efficiency between 0 and 1. the pumping efficiency ratio.
#' @param opts
#'   List of simulation parameters returned by the function \code{antaresRead::setSimulationPath}
#' @param max_hydro weekly pumping and turbining maximum powers, generated by \code{get_max_hydro}
#' @param inflow weekly inflow
#' @param mcyears Vector of years used to evaluate inflow
#'
#' @export
constraint_generator <- function(area,nb_disc_stock,pumping=F,pumping_efficiency=NULL,opts,max_hydro=NULL,
                                 inflow=NULL,mcyears=NULL){



  if(is.null(pumping_efficiency)){
    pumping_efficiency <- getPumpEfficiency(area,opts=opts)
  }

  if(is.null(max_hydro)){
    max_hydro <- get_max_hydro(area,opts,timeStep = "weekly")
  }
  res_cap <- get_reservoir_capacity(area,opts)
  if (is.null(inflow)){
    assertthat::assert_that(!is.null(mcyears))
    inflow <- get_inflow(area=area, opts=opts,mcyears=mcyears)
  }
  max_app <- inflow %>%
    dplyr::group_by(.data$timeId) %>%
    dplyr::summarise(max_app=max(.data$hydroStorage))
  max_hydro <- dplyr::left_join(max_hydro,max_app,by=c("timeId"))

  weeks <- dplyr::distinct(max_hydro,.data$timeId)$timeId
  df_constraint <- data.frame(week=weeks)
  df_constraint$u <- sapply(df_constraint$week,
                            FUN = function(w) constraint_week(pumping,
                                      pumping_efficiency,
                                      nb_disc_stock,res_cap,
                                      dplyr::filter(max_hydro,
                                                    .data$timeId==w),w),
                            simplify = F)

  df_constraint <- tidyr::unnest_wider(df_constraint,.data$u,names_sep = "_")
  df_constraint <- df_constraint %>%
    tidyr::pivot_longer(cols=2:length(df_constraint),names_to="sim",values_to="u") %>%
    dplyr::arrange(.data$week,.data$u)

  return(df_constraint)
}


#' Generate constraint values for a week at a time used in \code{constraint_generator}
#' depending on the maximum powers for the week
#'
#' @param pumping Boolean. True to take into account the pumping.
#' @param pumping_efficiency between 0 and 1. the pumping efficiency ratio.
#' @param nb_disc_stock Number of constraint values wanted for each week
#' @param res_cap Double, reservoir capacity
#' @param hydro Pumping and turbining maximum powers for the week, generated by \code{get_max_hydro}
#' @param week Current week for which to compute controls
#'
#' @return List of constraint values for the week
constraint_week <- function(pumping,pumping_efficiency,nb_disc_stock,res_cap,hydro,week){
  maxi <- min(hydro$turb,res_cap+hydro$max_app)
  mini <- max(-res_cap,-hydro$pump*pumping_efficiency)

  if(pumping){
    if(nb_disc_stock<3){
      if (week==1){
        message("nb_disc_stock should be greater than 2")
      }
      constraint_values <- c(0)
    } else {
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
    }


  }else{
    if(nb_disc_stock<2){
      if (week==1){
        message("nb_disc_stock should be greater than 1")
      }
      constraint_values <- c(0)
    } else {
      constraint_values <- seq(from = 0, to = maxi, length.out = nb_disc_stock)
      constraint_values <- round(constraint_values)
    }

  }
  return(constraint_values)
}

#' Generate coefficients for biding constraints, used in \code{runWaterValuesSimulation}
#'
#' @param area Area with the area
#' @param fictive_area Fictive area involved in the biding contraint
#'
#' @return Named vector of coefficients
generate_link_coeff <- function(area,fictive_area){

  if (match(area, sort(c(area, fictive_area))) == 1) {
    coeff <- stats::setNames(-1, paste(area, fictive_area, sep = "%"))
  } else {
    coeff <- stats::setNames(1, paste(fictive_area, area, sep = "%"))
  }

  return(coeff)
}
