
#---------Plot reward variation--------
#' Plot the reward variation and return the results in table
#'
#' @param reward_base A data.table contains the rewards.
#' Obtained using the function get_Reward()
#' @param week_id Numeric of length 1. number of the week to plot.
#'
#' @import ggplot2


plot_reward_variation <- function(reward_base,week_id)
{
  reward <- aggregate(reward_base[,3:ncol(reward_base)],list(reward_base$timeId),mean)
  reward$Group.1 <- NULL
  temp <- diff(unlist(reward[week_id,]))
  t <- seq(from=1,to=length(temp))
  temp <- data.frame(t,temp)
  setnames(temp,"temp","Reward Transition")
  setnames(temp,"t","Turbining transistion")
  p1 <- ggplot(data = temp,aes(`Turbining transistion` , `Reward Transition`)) +geom_line(size=1,color="purple 4")
  print(p1)
  return(temp)
}

#--------- Plot reward -----------
#' Plot the reward and return the results in table
#'
#' @param reward_base A data.table contains the rewards.
#' Obtained using the function get_Reward()
#' @param week_id Numeric of length 1. number of the week to plot.
#'
#' @import ggplot2

plot_reward <- function(reward_base,week_id)
{
  reward <- aggregate(reward_base[,3:ncol(reward_base)],list(reward_base$timeId),mean)
  reward$Group.1 <- NULL
  temp <- (unlist(reward[week_id,]))
  t <- seq(from=1,to=length(temp))
  temp <- data.frame(t,temp)
  setnames(temp,"temp","Reward")
  setnames(temp,"t","Turbining capacity")
  p1 <- ggplot(data = temp,aes(`Turbining capacity` , `Reward`)) +geom_line(size=1,color="purple 4")
  print(p1)
  return(temp)
}


#----------Bellman Plot--------------
#' Plot Bellman and Water values
#'
#' @param value_nodes_dt A data.table contains the Bellman and water values .
#' Obtained using the function Grid_Matrix()
#' @param week_id Numeric of length 1. number of the week to plot.
#' @param param string contains the element to plot
#'   * "vu" to plot only water values
#'   * "b" to plot only bellman values
#'   * "both" to plot both water and bellman values
#'   Default "vu"
#'
#' @import ggplot2


plot_Bellman <- function(value_nodes_dt,week_number,param="vu"){

  temp <- value_nodes_dt[weeks ==week_number]
  temp <- temp[is.finite(vu)&(!is.nan(vu))]

  p1 <- ggplot(data = temp, aes(states , vu)) +geom_line(size=1,color="purple 4")

  setnames(temp,"value_node","Bellman_Value")
  p2 <- ggplot(data = temp, aes(states ,Bellman_Value)) +geom_line(size=1,color="red 4")

  if (param=="vu") {
    print(p1)
  }else if(param=="both") {

    tit <- sprintf("VU and Bellman for Week %d",week_number)
    title <- ggdraw() + draw_label(tit, fontface='bold')
    p <- plot_grid(p1,p2)
    plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1)) # rel_heights values control title margins
  }else print(p2)

}


#--------- Reservoir Guide graph Plot---------------
#' Plot Reservoir Guide Graph and return result table
#'
#' @param area An 'antares' area.
#' @param timeStep Resolution of the data to import:
#' weekly (default, a linear interpolation is done on the data),
#' monthly (original data).
#' @param mcyear precise the MC year to plot.
#' Null plot the synthesis. Default NULL
#' @param simulation_name simulation name to plot.
#' @param opts
#'   List of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#'
#' @import ggplot2
#' @import readReservoirLevels
#' @import dplyr
#' @import antaresRead


plot_reservoir <- function(area,timeStep="weekly",mcyear=NULL,simulation_name=NULL,opts=antaresRead::simOptions()){

reservoir <- readReservoirLevels(area, timeStep = timeStep, byReservoirCapacity = FALSE, opts = opts)
reservoir$level_avg <- NULL
reservoir$level_high <- reservoir$level_high*100
reservoir$level_low <- reservoir$level_low*100

if(is.null(simulation_name)){

  sim_names <- getSimulationNames("",opts = opts)
  for (i in 1:length(sim_names))
   { t <- sprintf("[%d] ==> %s",i,sim_names[i])
    cat(t,sep="\n")}

  sim_nb <- 0
  while(sim_nb < 1|(sim_nb >length(sim_names)))
    {sim_nb <- readline(prompt="Enter simulation number: ")
    sim_nb <- as.integer(sim_nb)
    }
  simulation_name <- sim_names[sim_nb]

 }


#read reservoir actual levels:
tmp_opt <- setSimulationPath(path = opts$studyPath, simulation = simulation_name)
inflow <- readAntares(areas = area, hydroStorage = TRUE, timeStep = timeStep , mcYears = mcyear, opts = tmp_opt)
if (!is.null(mcyear)){
  inflow <- inflow[order(mcYear, timeId)]
  inflow <- inflow[, list(timeId,`H. LEV` )]
}else{
  inflow <- inflow[order(timeId)]
  inflow <- inflow[, list(timeId,`H. LEV` )]
  }



temp <- left_join(x=reservoir,y=inflow,by="timeId")
p <- ggplot(data=temp, aes(x=timeId)) +
 geom_line(aes(y = level_low ), color = "darkred") +
  geom_line(aes(y = level_high ), color="darkred")+
  geom_line(aes(y = `H. LEV` ), color="darkblue")

print(p)

return(temp)
}
