
#---------Plot reward variation--------
#' Plot the reward variation and return the results in table
#'
#' @param reward_base A data.table contains the rewards.
#' Obtained using the function get_Reward()
#' @param week_id Numeric of length 1. number of the week to plot.
#' @param constraints_values the value of the constraint
#' @param output Boolean. TRUE to return the table of values with the plot.
#' @importFrom grDevices rgb
#' @importFrom stats aggregate
#' @importFrom ggplot2 aes element_text geom_line ggplot ggtitle theme
#' @return a \code{ggplot} object
#' @export


plot_reward_variation <- function(reward_base,week_id,constraints_values=NULL,output=FALSE)
{


  reward <- stats::aggregate(reward_base[,3:ncol(reward_base)],list(reward_base$timeId),mean)
  reward$Group.1 <- NULL
  temp <- reward[week_id,]
  temp <- as.data.table(t(temp))

  temp$u <- as.double(str_extract(colnames(reward_base)[3:length(reward_base)], "\\-?\\d+$"))
  temp <- arrange(temp,u)
  temp$u <- NULL


  t <- seq(from=1,to=(nrow(temp)-1))
  temp <- sapply(temp, diff)
  temp <- data.table(t,temp)
  setnames(temp,"t","Turbining transistion")

  temp <- melt(temp,id.vars="Turbining transistion",variable.name="week")
  setnames(temp,"value","Reward transition")
  tryCatch({if(!is.null(constraints_values)){
    energy_quantity <- diff(constraints_values)
    temp$`Reward transition` <-  temp$`Reward transition`/energy_quantity}},
    error=function(e){message("invalid constraints_values")})

  temp$`Reward transition` <- round(temp$`Reward transition`,digits = 2)


  p1 <- ggplot2::ggplot(data = temp,ggplot2::aes(x=`Turbining transistion`,`Reward transition`, col=week)) +ggplot2::geom_line(size=0.5)
  p1 <- p1+ggplot2::ggtitle(sprintf("Reward variation"))+ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
  if(length(unique(temp$week))>10){
    p1 <- p1+ggplot2:: theme(legend.position="none")
  }
  print(p1)

  temp <-  dplyr::relocate(temp,week, .before = 1)
  output <- list()
  output$graph <- p1
  output$table <- temp
  return(output)
}

#--------- Plot reward -----------
#' Plot the reward and return the results in table
#'
#' @param reward_base A data.table contains the rewards.
#' Obtained using the function get_Reward()
#' @param week_id Numeric of length 1. number of the week to plot.
#' @param constraints_values the value of the constraint
#' @param output Boolean. TRUE to return the table of values with the plot.
#' @param sim_name_pattern the name of simulations used in \code{runWaterValuesSimulation()}
#' @importFrom stats aggregate
#' @importFrom ggplot2 aes element_text geom_line ggplot ggtitle theme
#' @importFrom grDevices rgb
#' @return a \code{ggplot} object
#' @export

plot_reward <- function(reward_base,week_id,sim_name_pattern="weekly_water_amount_",constraints_values=NULL,output=FALSE)
{
  t <- seq(1,(length(colnames(reward_base))-2))

  tryCatch({if(!is.null(constraints_values)){
    t <- constraints_values
  }}, error=function(e){message("invalid constraints_values")})

  reward <- stats::aggregate(reward_base[,3:ncol(reward_base)],list(reward_base$timeId),mean)
  reward$Group.1 <- NULL
  temp <- reward[week_id,]
  temp <- as.data.table(t(temp))



  if(max(t)>100000){
    temp$"Turbining capacity GWh" <- as.double(str_extract(colnames(reward_base)[3:length(reward_base)], "\\-?\\d+$"))
    temp <- melt(temp,id.vars="Turbining capacity GWh",variable.name="week")
    setnames(temp,"value","Reward")
    p1 <- ggplot2::ggplot(data = temp,ggplot2::aes(x=`Turbining capacity GWh`,Reward, col=week)) +ggplot2::geom_line(size=0.5)
  }else{
    temp$"Turbining capacity" <- as.double(str_extract(colnames(reward_base)[3:length(reward_base)], "\\-?\\d+$"))*1000
    temp <- melt(temp,id.vars="Turbining capacity",variable.name="week")
    setnames(temp,"value","Reward")
    p1 <- ggplot2::ggplot(data = temp,ggplot2::aes(x=`Turbining capacity`,Reward, col=week)) +ggplot2::geom_line(size=0.5)
  }
  p1 <- p1+ggplot2::ggtitle(sprintf("Reward week"))+ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))

  if(length(unique(temp$week))>10){
     p1 <- p1+ggplot2:: theme(legend.position="none")
    }
  print(p1)
  temp <-  dplyr::relocate(temp,week, .before = 1)
  temp$Reward <- round(temp$Reward)
  output <- list()
  output$graph <- p1
  output$table <- temp
  return(output)
}


#--------- Plot reward by MC year -----------
#' Plot the reward and return the results in table
#'
#' @param reward_base A data.table contains the rewards.
#' Obtained using the function get_Reward()
#' @param week_id Numeric of length 1. number of the week to plot.
#' @param Mc_year Numeric of length 1. number of thr MC year to plot
#' @param sim_name_pattern the name of simulations used in \code{runWaterValuesSimulation()}
#' @param constraints_values the value of the constraint
#' @importFrom grDevices rgb
#' @importFrom ggplot2 aes element_text geom_line ggplot ggtitle theme
#' @return a \code{ggplot} object
#' @export

plot_reward_mc <- function(reward_base,week_id,Mc_year,sim_name_pattern="weekly_water_amount_",constraints_values=NULL)
{

  t <- seq(1,(length(colnames(reward_base))-2))

  tryCatch({if(!is.null(constraints_values)){
    t <- constraints_values
  }}, error=function(e){message("invalid constraints_values")})


  reward <- reward_base[timeId %in% week_id&mcYear%in%Mc_year]
  names <- unlist(reward[,legend:=paste(sprintf("week %d",timeId),sprintf("MC year %d",mcYear))]$legend)
  temp <- reward[,3:ncol(reward_base)]

  temp <- as.data.table(t(temp))
  setnames(temp,colnames(temp),names)
  temp$"Turbining capacity" <- as.double(str_extract(colnames(reward_base)[3:length(reward_base)], "\\-?\\d+$"))*1000
  temp <- melt(temp,id.vars="Turbining capacity",variable.name="week")
  setnames(temp,"value","Reward")
  if(max(t)>100000){
    temp$"Turbining capacity GWh" <- round( temp$`Turbining capacity`/1000)
    temp$`Turbining capacity` <- NULL
    temp <-  dplyr::relocate(temp,`Turbining capacity GWh`, .before = 2)

    p1 <- ggplot2::ggplot(data = temp,ggplot2::aes(x=`Turbining capacity GWh`,Reward, col=week)) +ggplot2::geom_line(size=0.5)
  }else{
    p1 <- ggplot2::ggplot(data = temp,ggplot2::aes(x=`Turbining capacity`,Reward, col=week)) +ggplot2::geom_line(size=0.5)
    temp <-  dplyr::relocate(temp,`Turbining capacity`, .before = 2)

    }

  p1 <- p1+ggplot2::ggtitle(sprintf("Reward week  MC Year %s",paste(as.character(week_id),collapse =" ")))+ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))

 if(length(unique(temp$week))>10){
    p1 <- p1+ggplot2:: theme(legend.position="none")
  }
  print(p1)
  temp <-  dplyr::relocate(temp,week, .before = 1)
  temp$Reward <- round(temp$Reward)
  output <- list()
  output$graph <- p1
  output$table <- temp
  return(output)
}





#--------- Plot reward variation by MC year -----------
#' Plot the reward and return the results in table
#'
#' @param reward_base A data.table contains the rewards.
#' Obtained using the function get_Reward()
#' @param week_id Numeric of length 1. number of the week to plot.
#' @param Mc_year Numeric of length 1. number of thr MC year to plot
#' @param constraints_values the value of the constraint
#' @importFrom grDevices rgb
#' @importFrom ggplot2 aes element_text geom_line ggplot ggtitle theme
#' @return a \code{ggplot} object
#' @export

plot_reward_variation_mc <- function(reward_base,week_id,Mc_year,constraints_values=NULL)
{
  reward <- reward_base[timeId %in% week_id&mcYear%in%Mc_year]
  names <- unlist(reward[,legend:=paste(sprintf("week %d",timeId),sprintf("MC year %d",mcYear))]$legend)
  temp <- reward[,3:ncol(reward_base)]

  temp <- as.data.table(t(temp))
  t <- seq(from=1,to=(nrow(temp)-1))

  temp$u <- as.double(str_extract(colnames(reward_base)[3:length(reward_base)], "\\-?\\d+$"))
  temp <- arrange(temp,u)
  temp$u <- NULL
  temp <- sapply(temp, diff)

  temp <- data.table(t,temp)
  setnames(temp,colnames(temp)[-1],names)
  setnames(temp,"t","Turbining transistion")
  temp <- melt(temp,id.vars="Turbining transistion",variable.name="week")
  setnames(temp,"value","Reward transition")


  tryCatch({if(!is.null(constraints_values)){
    energy_quantity <- diff(constraints_values)
    temp$`Reward transition` <-  temp$`Reward transition`/energy_quantity}},
    error=function(e){message("invalid constraints_values")})



  p1 <- ggplot2::ggplot(data = temp,ggplot2::aes(x=`Turbining transistion`,`Reward transition`, col=week)) +ggplot2::geom_line(size=0.5)
  p1 <- p1+ggplot2::ggtitle(sprintf("Reward variation  MC Year %s",paste(as.character(week_id),collapse =" ")))+ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
  if(length(unique(temp$week))>10){
    p1 <- p1+ggplot2:: theme(legend.position="none")
  }
  print(p1)
  temp <-  dplyr::relocate(temp,week, .before = 1)
  temp$`Reward transition` <- round(temp$`Reward transition`,digits = 2)
  output <- list()
  output$graph <- p1
  output$table <- temp
  return(output)
}


#----------Bellman Plot--------------
#' Plot Bellman and Water values
#'
#' @param value_nodes_dt A data.table contains the Bellman and water values .
#' Obtained using the function Grid_Matrix()
#' @param week_number Numeric of length 1. number of the week to plot.
#' @param param string contains the element to plot
#'   * "vu" to plot only water values
#'   * "b" to plot only bellman values
#'   * "both" to plot both water and bellman values
#'   Default "vu"
#' @param bellman_week Numeric of length 1. number of the week to plot
#' the correspondent Bellman values.
#' @param states_step_ratio put the ratio to change reservoir discretization in percent
#' 0.01 to augment by 1\%
#' @param ... further arguments passed to or from other methods.
#' @import data.table
#' @importFrom  cowplot draw_label ggdraw plot_grid
#' @importFrom ggplot2 aes element_text geom_line ggplot ggtitle theme
#' @importFrom grDevices rgb
#' @return a \code{ggplot} object
#' @export


plot_Bellman <- function(value_nodes_dt,week_number,param="vu",states_step_ratio=0.01,bellman_week=NULL,...){

  # if(week_number<52){
  #   next_week_number <- week_number+1
  # }else{
  #   next_week_number ==1
  # }
  next_week_number <- week_number

  if(param=="bell"){next_week_number <- week_number}
  if(is.numeric(bellman_week)){next_week_number <- bellman_week}
  temp <- value_nodes_dt[weeks ==next_week_number]

  temp$vu <- value_nodes_dt[weeks ==week_number]$vu

  temp <- states_to_percent(temp,states_step_ratio)

  temp <- temp[is.finite(vu)&(!is.nan(vu))]


  setnames(temp,"value_node","Bellman_Value")
  setnames(temp,"states_round_percent","Reservoir_percent")


  p1 <- ggplot2::ggplot(data = temp, ggplot2::aes(Reservoir_percent , vu)) +ggplot2::geom_line(size=1,color="purple 4")
  p1 <- p1+ggplot2::ggtitle(sprintf("Water Values"))+ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))

  p2 <- ggplot2::ggplot(data = temp, ggplot2::aes(Reservoir_percent ,Bellman_Value)) +ggplot2::geom_line(size=1,color="red 4")
  p2 <- p2+ggplot2::ggtitle(sprintf("Bellman Values %d",next_week_number))+ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))

  p3 <- ggplot2::ggplot(data = temp, ggplot2::aes(Reservoir_percent ,value_node_dif)) +ggplot2::geom_line(size=1,color="green 4")
  p3 <- p3+ggplot2::ggtitle(sprintf("Gradien Bellman %d",next_week_number))+ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))

  if (param=="vu") {
    return(p1)
  }else if(param=="both") {

    tit <- sprintf("VU for Week %d",week_number)
    title <- cowplot::ggdraw() + cowplot::draw_label(tit, fontface='bold')
    p <- cowplot::plot_grid(p1,p2)
    cowplot::plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1)) # rel_heights values control title margins
  }else if(param=="bell")
  {return(p2)
  }else {
    tit <- sprintf("VU for Week %d",week_number)
    title <- cowplot::ggdraw() + cowplot::draw_label(tit, fontface='bold')
    p <- cowplot::plot_grid(p1,p2,p3)
    cowplot::plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1))
    return(p)
  }


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
#' @param shiny Boolean. True to run the script in shiny mod.
#' @param only_g Boolean. True plot only constraints graph.
#' @import data.table
#' @importFrom ggplot2 aes element_text geom_line ggplot ggtitle scale_color_manual theme
#' @importFrom dplyr left_join
#' @importFrom tidyr pivot_wider
#' @importFrom grDevices rgb
#' @importFrom  antaresRead setSimulationPath readAntares
#' @return a \code{ggplot} object
#' @export


plot_reservoir <- function(area,timeStep="weekly",mcyear=NULL,simulation_name=NULL,opts=antaresRead::simOptions(),shiny=F,only_g=F){

  reservoir <- readReservoirLevels(area, timeStep = timeStep, byReservoirCapacity = FALSE, opts = opts)
  reservoir$level_avg <- NULL
  reservoir$level_high <- reservoir$level_high*100
  reservoir$level_low <- reservoir$level_low*100


  if(!shiny){
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

    }}


  #read reservoir actual levels:
  tmp_opt <- antaresRead::setSimulationPath(path = opts$studyPath, simulation = simulation_name)
  inflow <- antaresRead::readAntares(areas = area, timeStep = timeStep , mcYears = mcyear, opts = tmp_opt)

  if(is.null(mcyear)){
    inflow <- inflow[order(timeId)]
    inflow <- inflow[, list(timeId,`H. LEV` )]
    temp <- dplyr::left_join(x=reservoir,y=inflow,by="timeId")
    p <- ggplot2::ggplot(data=temp, ggplot2::aes(x=timeId)) +
      ggplot2::geom_line(ggplot2::aes(y = level_low ), color = "red") +
      ggplot2::geom_line(ggplot2::aes(y = level_high ), color="red")
    if(!only_g){
      p <- p+ggplot2::geom_line(ggplot2::aes(y = `H. LEV` ), color="blue")}
    p <- p+ggplot2::ggtitle(sprintf("%s Reservoir Path for MC synthesis",area))+ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))

    print(p)
    return(p)
  }else{
    inflow <- inflow[order(mcYear, timeId)]
    inflow <- inflow[, list(mcYear,timeId,`H. LEV` )]
    d <- tidyr::pivot_wider(inflow, names_from = mcYear, values_from = "H. LEV")
    temp1 <- dplyr::left_join(x=reservoir,y=d,by="timeId")
    temp <- melt(temp1, id.vars="timeId")

  }

  if(is.numeric(mcyear)&(length(mcyear)==1)){
    mc <- sprintf("MC_year")
    old <- colnames(temp1)
    setnames(temp1,old[4],mc)
    temp1 <- temp1[,list(timeId,level_low,MC_year,level_high)]

    p <- ggplot2::ggplot(data=temp1, ggplot2::aes(x=timeId)) +
      ggplot2::geom_line(ggplot2::aes(y = level_low ), color = "red") +
      ggplot2::geom_line(ggplot2::aes(y = level_high ), color="red")+
    if(!only_g){
      p <- p+ggplot2::geom_line(ggplot2::aes(y =MC_year ), color="blue")}
    p <- p+ggplot2::ggtitle(sprintf("%s Reservoir Path for MC year %d",area,mcyear))+ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))


  }else{
    p <- ggplot2::ggplot(temp, ggplot2::aes(x = timeId, y = value, colour = variable)) +
      ggplot2::geom_line(lwd=1) + ggplot2::scale_color_manual(values =c("level_low" = "red",
                                                                        "level_high" = "red"))
    p <- p+ggplot2::ggtitle(sprintf("%s Reservoir Path",area))+ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))

  }
  print(p)

  return(p)






}


#--------- Turbining power graph Plot---------------
#' Plot Turbining power  Graph and return result table
#'
#' @param area An 'antares' area.
#' @param timestep Resolution of the data to import:
#' weekly (default, a linear interpolation is done on the data),
#' monthly (original data).
#' @param Mcyear precise the MC year to plot.
#' Null plot the synthesis. Default NULL
#' @param min_path path of Pmin file "/user/Pmin **.txt"
#' @param max_path path of Pmax file "/user/Pmax **.txt"
#' @param simulation_name simulation name to plot.
#' @param opts
#'   List of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#' @import data.table
#' @importFrom ggplot2 ggplot geom_line ggtitle labs theme
#' @importFrom  dplyr left_join
#' @importFrom  tidyr pivot_wider
#' @importFrom  antaresRead setSimulationPath readAntares
#' @importFrom stats setNames aggregate
#' @importFrom grDevices rgb
#' @return a \code{ggplot} object
#' @export




plot_generation <- function(area,timestep="daily",Mcyear=NULL,min_path,max_path,
                            simulation_name=NULL,opts=antaresRead::simOptions())


{

  Pmin <- read.table(min_path, header = FALSE, sep = "", dec = ".")
  Pmax <- read.table(max_path, header = FALSE, sep = "", dec = ".")
  Pmax <- Pmax[-365,]


  Pmin$hours <- seq(length.out=nrow(Pmin))
  tmp <- NULL
  for(i in 1:365){
    tmp <- append(tmp,rep(i,24))
  }
  Pmin$day <- tmp
  Pmin <- Pmin[Pmin$day<365,]


  #---- Select simulation---
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

  tmp_opt <- antaresRead::setSimulationPath(path = opts$studyPath, simulation = simulation_name)

  #----- Read hydro generation power
  P <- antaresRead::readAntares(areas = area, timeStep = timestep,
                                mcYears = Mcyear, opts=tmp_opt,select = "H. STOR" )
  ncol <- ncol(Pmin)
  if(is.null(Mcyear)){
    P <- P[order(timeId)]
    P <- P[, list(timeId,`H. STOR` )]
    Pmin <- Pmin  %>% select(ncol-4,ncol-1,ncol)
    Pmin <- stats::setNames(Pmin,c("Pmin","hour","day"))
  }else{
    P <- P[order(P$mcYear, P$timeId),]
    P <- P[,list(mcYear,timeId,`H. STOR` )]
    P$mcYear <- NULL
    Pmin <-  dplyr::select(Pmin,Mcyear,ncol-1,ncol)
    Pmin <- stats::setNames(Pmin,c("Pmin","hour","day"))
  }






  if(timestep=="hourly"){
    Pmax$hourly <- Pmax$V1
    Pmax_hourly <- NULL
    for(i in 1:nrow(Pmax)){
      Pmax_hourly <- append(Pmax_hourly,rep(Pmax$hourly[i],24))
    }
    generation_hourly <-data.frame(P$timeId)
    setnames(x = generation_hourly,"P.timeId","hour")
    generation_hourly$Pmax <- Pmax_hourly
    generation_hourly$generation <- P$`H. STOR`
    Pmin$day <- NULL
    generation_hourly <- dplyr::left_join(generation_hourly,Pmin,by="hour")

    p <- ggplot2::ggplot(data=generation_hourly, aes(x=hour)) +
      ggplot2::geom_line(aes(y = Pmin ), color = "red") +
      ggplot2::geom_line(aes(y = Pmax ), color="red")+
      ggplot2::geom_line(aes(y =generation ), color="blue")
    if(is.null(Mcyear))
    { p <- p+ggplot2::ggtitle(sprintf("Hourly Generation for Synthesis year"))
    }else{
      p <- p+ggplot2::ggtitle(sprintf("Hourly Generation for MC year %d",Mcyear))
    }
    p <- p+ggplot2::theme(plot.title = element_text(hjust = 0.5))
    p <- p+ ggplot2::labs(x = "hour",
                 y = "MWh")
    otp <- generation_hourly
  }

  if(timestep=="daily"){
    generation_daily <-data.frame(P$timeId)
    setnames(x = generation_daily,"P.timeId","day")
    generation_daily$Pmax <- Pmax$V1*Pmax$V2
    generation_daily$generation <- P$`H. STOR`
    t <- stats::aggregate(Pmin~day, data=Pmin, FUN=sum)
    generation_daily$Pmin <- t$Pmin

    p <- ggplot2::ggplot(data=generation_daily, aes(x=day)) +
      ggplot2::geom_line(aes(y = Pmin ), color = "red") +
      ggplot2::geom_line(aes(y = Pmax ), color="red")+
      ggplot2::geom_line(aes(y =generation ), color="blue")
    if(is.null(Mcyear))
    { p <- p+ggplot2::ggtitle(sprintf("Daily Generation for Synthesis year"))
    }else{
      p <- p+ggplot2::ggtitle(sprintf("Daily Generation for MC year %d",Mcyear))
      }

    p <- p+ggplot2::theme(plot.title = element_text(hjust = 0.5))
    p <- p+ ggplot2::labs(x = "day",
                 y = "MWh")
    otp <- generation_daily
    }
  print(p)
  return(p)
  }


#--------- Reporting graph Plot---------------
#' Plot simulation variables comparison and real Ov. cost (for watervalues)
#'
#' @param data A data.table contains the simulation results and  real Ov. cost.
#' Obtained using the function plot_results()
#' @param plot_var list of variables to plot.
#' @param plot_type boolean. True to plot by area. False to plot by simulation
#' @importFrom  ggplot2 ggplot geom_col scale_fill_viridis_d facet_grid scale_fill_brewer
#' @importFrom dplyr select
#' @importFrom grDevices rgb
#' @return a \code{ggplot} object
#' @export


just_plot_report <- function(data,plot_var,plot_type=T){

  if(plot_type) {
    var <- "sim_name"
  }else{
    var <- "area"
  }


  data1 <- dplyr::select(data,append(plot_var,var))
  fin_data <-  melt(data1, id.vars=var)

  if(plot_type) {
    p <-  ggplot2::ggplot(data=fin_data, aes(x=sim_name, y=value, fill=sim_name))
  }else{
    p <-  ggplot2::ggplot(data=fin_data, aes(x=area, y=value, fill=area))
  }
    p <- p+ggplot2::geom_col() +
    ggplot2::scale_fill_viridis_d() +
    ggplot2::facet_grid(. ~ variable)+
    ggplot2::scale_fill_brewer(palette="Paired")
  print(p)

  return(p)
}




#--------- Water Flow Plot---------------
#' Plot water flow Graph
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
#' @param shiny Boolean. True to run the script in shiny mod.
#' @param ... further arguments passed to or from other methods.
#' @import data.table
#' @importFrom ggplot2 aes element_text geom_line ggplot ggtitle scale_color_manual theme
#' @importFrom dplyr left_join
#' @importFrom tidyr pivot_wider
#' @importFrom grDevices rgb
#' @importFrom  antaresRead setSimulationPath readAntares
#' @return a \code{ggplot} object
#' @export


plot_flow <- function(area,timeStep="weekly",mcyear=NULL,simulation_name=NULL,opts=antaresRead::simOptions(),shiny=F,...){


  if(!shiny){
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

    }}


  #read reservoir actual levels:
  tmp_opt <- antaresRead::setSimulationPath(path = opts$studyPath, simulation = simulation_name)
  inflow <- antaresRead::readAntares(areas = area, timeStep = timeStep ,
                                     mcYears = mcyear, opts = tmp_opt, hydroStorage = T)

  if(is.null(mcyear)){
    inflow <- inflow[order(timeId)]
    inflow <- inflow[, list(timeId,hydroStorage )]
    temp <- inflow
    p <- ggplot2::ggplot(data=temp, ggplot2::aes(x=timeId)) +
      ggplot2::geom_line(ggplot2::aes(y = hydroStorage ), color="blue")
    p <- p+ggplot2::ggtitle(sprintf("%s Water flow Path for MC synthesis",area))+ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))

    print(p)
    return(p)
  }else{
    inflow <- inflow[order(mcYear, timeId)]
    inflow <- inflow[, list(mcYear,timeId,hydroStorage )]
    d <- tidyr::pivot_wider(inflow, names_from = mcYear, values_from = "hydroStorage")
    temp1 <- as.data.table(d)
    temp <- melt(temp1, id.vars="timeId")

  }

  if(is.numeric(mcyear)&(length(mcyear)==1)){
    mc <- sprintf("Water_Flow")
    old <- colnames(temp1)
    setnames(temp1,old[2],mc)
    temp1 <- temp1[,list(timeId,Water_Flow)]

    p <- ggplot2::ggplot(data=temp1, ggplot2::aes(x=timeId)) +
      ggplot2::geom_line(ggplot2::aes(y =Water_Flow ), color="blue")
    p <- p+ggplot2::ggtitle(sprintf("%s Water Flow for MC year %d",area,mcyear))+ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))


  }else{
    names <- unlist(temp[,legend:=sprintf("MC year %d",variable)]$legend)

    p <- ggplot2::ggplot(data = temp,ggplot2::aes(x=timeId,value, col=legend)) +ggplot2::geom_line(size=0.5)
    p <- p+ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
    p <- p+ggplot2::ggtitle(sprintf("%s Water Flow for MC years",area))+ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))

  }
  print(p)

  return(p)






}

