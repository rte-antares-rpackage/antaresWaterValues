
#---------Plot reward variation--------
#' Plot the reward variation and return the results in table
#'
#' @param reward_base A data.table contains the rewards.
#' Obtained using the function get_Reward()
#' @param week_id Numeric of length 1. number of the week to plot.
#'
#' @import ggplot2


plot_reward_variation <- function(reward_base,week_id,sim_name_pattern="weekly_water_amount_")
{
  reward <- aggregate(reward_base[,3:ncol(reward_base)],list(reward_base$timeId),mean)
  reward$Group.1 <- NULL
  temp <- diff(unlist(reward[week_id,]))
  t <- seq(from=1,to=length(temp))
  temp <- data.frame(t,temp)
  setnames(temp,"temp","Reward Transition")
  setnames(temp,"t","Turbining transistion")
  p1 <- ggplot(data = temp,aes(`Turbining transistion` , `Reward Transition`)) +geom_line(size=1,color="red 4")
  p1 <- p1+ggtitle(sprintf("Reward variation week %d",week_id))+theme(plot.title = element_text(hjust = 0.5))
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

plot_reward <- function(reward_base,week_id,sim_name_pattern="weekly_water_amount_")
{
  t <- names_reward(reward_base,simulation_name_pattern)
  reward <- aggregate(reward_base[,3:ncol(reward_base)],list(reward_base$timeId),mean)
  reward$Group.1 <- NULL
  temp <- (unlist(reward[week_id,]))
  temp <- data.frame(t,temp)
  setnames(temp,"temp","Reward")
  setnames(temp,"t","Turbining capacity")
  p1 <- ggplot(data = temp,aes(`Turbining capacity` , `Reward`)) +geom_line(size=1,color="purple 4")
  p1 <- p1+ggtitle(sprintf("Reward week %d",week_id))+theme(plot.title = element_text(hjust = 0.5))
  print(p1)
  return(temp)
}

#--------- Plot reward by MC year -----------
#' Plot the reward and return the results in table
#'
#' @param reward_base A data.table contains the rewards.
#' Obtained using the function get_Reward()
#' @param week_id Numeric of length 1. number of the week to plot.
#' @param Mc_year Numeric of length 1. number of thr MC year to plot
#'
#' @import ggplot2

plot_reward_mc <- function(reward_base,week_id,Mc_year,sim_name_pattern="weekly_water_amount_")
{
  t <- names_reward(reward_base,simulation_name_pattern)
  reward <- reward_base[timeId==week_id&mcYear==Mc_year]
  reward <- reward[,3:ncol(reward_base)]
  temp <- (unlist(reward))
  temp <- data.frame(t,temp)
  setnames(temp,"temp","Reward")
  setnames(temp,"t","Turbining capacity")
  p1 <- ggplot(data = temp,aes(`Turbining capacity` , `Reward`)) +geom_line(size=1,color="purple 4")
  p1 <- p1+ggtitle(sprintf("Reward week %d MC Year %d",week_id,Mc_year))+theme(plot.title = element_text(hjust = 0.5))
  print(p1)
  return(temp)
}
#--------- Plot reward variation by MC year -----------
#' Plot the reward and return the results in table
#'
#' @param reward_base A data.table contains the rewards.
#' Obtained using the function get_Reward()
#' @param week_id Numeric of length 1. number of the week to plot.
#' @param Mc_year Numeric of length 1. number of thr MC year to plot
#'
#' @import ggplot2

plot_reward_variation_mc <- function(reward_base,week_id,Mc_year,sim_name_pattern="weekly_water_amount_")
{
  reward <- reward_base[timeId==week_id&mcYear==Mc_year]
  reward <- reward[,3:ncol(reward_base)]
  temp <- diff(unlist(reward))
  t <- names_reward(reward_base,simulation_name_pattern)
  t <- t[t!=0]
  temp <- data.frame(t,temp)
  setnames(temp,"temp","Reward Transition")
  setnames(temp,"t","Turbining transistion")
  p1 <- ggplot(data = temp,aes(`Turbining transistion` , `Reward Transition`)) +geom_line(size=1,color="red 4")
  p1 <- p1+ggtitle(sprintf("Reward variation week %d MC Year %d",week_id,Mc_year))+theme(plot.title = element_text(hjust = 0.5))

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
#' @param states_step_ratio put the ratio to change reservoir discretization in percent
#' 0.01 to augment by 1%
#' @import ggplot2


plot_Bellman <- function(value_nodes_dt,week_number,param="vu",states_step_ratio=0.01){

  temp <- value_nodes_dt[weeks ==week_number]

  temp <- states_to_percent(temp,states_step_ratio)

  temp <- temp[is.finite(vu)&(!is.nan(vu))]


  setnames(temp,"value_node","Bellman_Value")
  setnames(temp,"states_round_percent","Reservoir_percent")


  p1 <- ggplot(data = temp, aes(Reservoir_percent , vu)) +geom_line(size=1,color="purple 4")
  p1 <- p1+ggtitle(sprintf("Water Values"))+theme(plot.title = element_text(hjust = 0.5))

  p2 <- ggplot(data = temp, aes(Reservoir_percent ,Bellman_Value)) +geom_line(size=1,color="red 4")
  p2 <- p2+ggtitle(sprintf("Bellman Values"))+theme(plot.title = element_text(hjust = 0.5))

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
#' all to plot all years.
#' Null plot the synthesis. Default NULL
#' @param simulation_name simulation name to plot.
#' @param opts
#'   List of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#'
#' @import ggplot2
#' @importFrom watervalues readReservoirLevels
#' @import dplyr
#' @import antaresRead
#' @export


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

if(is.null(mcyear)){
  inflow <- readAntares(areas = area, timeStep = timeStep , mcYears = mcyear, opts = tmp_opt)
  inflow <- inflow[order(timeId)]
  inflow <- inflow[, list(timeId,`H. LEV` )]
  temp <- left_join(x=reservoir,y=inflow,by="timeId")
  p <- ggplot(data=temp, aes(x=timeId)) +
    geom_line(aes(y = level_low ), color = "red") +
    geom_line(aes(y = level_high ), color="red")+
    geom_line(aes(y = `H. LEV` ), color="blue")
  p <- p+ggtitle(sprintf("%s Reservoir Path for MC synthesis",area))+theme(plot.title = element_text(hjust = 0.5))

  print(p)
  return(temp)
}else{
  inflow <- readAntares(areas = area, timeStep = timeStep , mcYears = "all", opts = tmp_opt)
  inflow <- inflow[order(mcYear, timeId)]
  inflow <- inflow[, list(mcYear,timeId,`H. LEV` )]
  d <- pivot_wider(inflow, names_from = mcYear, values_from = "H. LEV")
  temp1 <- left_join(x=reservoir,y=d,by="timeId")
  temp <- melt(temp1, id.vars="timeId")

}

if(is.numeric(mcyear)){
  mc <- sprintf("MC_year")
  setnames(temp1,mcyear,mc)
  temp1 <- temp1[,list(timeId,level_low,MC_year,level_high)]

  p <- ggplot(data=temp1, aes(x=timeId)) +
    geom_line(aes(y = level_low ), color = "red") +
    geom_line(aes(y = level_high ), color="red")+
    geom_line(aes(y =MC_year ), color="blue")
  p <- p+ggtitle(sprintf("%s Reservoir Path for MC year %d",area,mcyear))+theme(plot.title = element_text(hjust = 0.5))


}else{
  p <- ggplot(temp, aes(x = timeId, y = value, colour = variable)) +
    geom_line(lwd=1) + scale_color_manual(values =c("level_low" = "red",
                                                    "level_high" = "red"))
  p <- p+ggtitle(sprintf("%s Reservoir Path for all MC year ",area))+theme(plot.title = element_text(hjust = 0.5))

}
print(p)

return(temp1)






}
