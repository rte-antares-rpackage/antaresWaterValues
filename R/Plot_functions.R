
#---------Plot reward variation--------
#' Plot the reward variation and return the results in table
#'
#' @param reward_base A data.table contains the rewards.
#' Obtained using the function get_Reward()
#' @param week_id Numeric of length 1. number of the week to plot.
#' @return a \code{ggplot} object
#' @export


plot_reward_variation <- function(reward_base,week_id)
{
  temp <- reward_base %>%
    dplyr::filter(.data$timeId %in% week_id) %>%
    dplyr::group_by(.data$timeId,.data$control) %>%
    dplyr::summarise(Reward=mean(.data$reward),.groups = "drop") %>%
    dplyr::mutate(week=as.character(.data$timeId)) %>%
    dplyr::select(-c("timeId")) %>%
    dplyr::group_by(week) %>%
    dplyr::arrange(.data$week,.data$control) %>%
    dplyr::mutate("Reward transition"=(dplyr::lead(.data$Reward)-.data$Reward)/(dplyr::lead(.data$control)-.data$control)) %>%
    dplyr::rename(`Turbining transistion`="control") %>%
    tidyr::drop_na()

  temp$`Reward transition` <- round(temp$`Reward transition`,digits = 2)


  p1 <- ggplot2::ggplot(data = temp,ggplot2::aes(x=.data$`Turbining transistion`,.data$`Reward transition`, col=week)) +ggplot2::geom_line(size=0.5)
  p1 <- p1+ggplot2::ggtitle(sprintf("Reward variation"))+ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
  if(length(unique(temp$week))>10){
    p1 <- p1+ggplot2::theme(legend.position="none")
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
#'
#' @return a \code{ggplot} object
#' @export

plot_reward <- function(reward_base,week_id)
{
  temp <- reward_base %>%
    dplyr::filter(.data$timeId %in% week_id) %>%
    dplyr::group_by(.data$timeId,.data$control) %>%
    dplyr::summarise(Reward=mean(.data$reward),.groups = "drop") %>%
    dplyr::mutate(week=as.character(.data$timeId))

  if(max(temp$control)>100000){
    temp$"Turbining capacity GWh" <- temp$control/1000
    p1 <- ggplot2::ggplot(data = temp,ggplot2::aes(x=.data$`Turbining capacity GWh`,.data$Reward, col=week)) +ggplot2::geom_line(size=0.5)
  }else{
    temp$"Turbining capacity" <- temp$control
    p1 <- ggplot2::ggplot(data = temp,ggplot2::aes(x=.data$`Turbining capacity`,.data$Reward, col=week)) +ggplot2::geom_line(size=0.5)
  }
  p1 <- p1+ggplot2::ggtitle(sprintf("Reward week"))+ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))

  if(length(unique(temp$week))>10){
     p1 <- p1+ggplot2::theme(legend.position="none")
    }
  print(p1)
  temp <-  dplyr::relocate(temp,week, .before = 1)
  temp$Reward <- round(temp$Reward)
  temp$control <- NULL
  temp$timeId <- NULL
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
#' @return a \code{ggplot} object
#' @export

plot_reward_mc <- function(reward_base,week_id,Mc_year)
{
  temp <- reward_base[reward_base$timeId %in% week_id&reward_base$mcYear%in%Mc_year] %>%
    dplyr::rename("Turbining capacity"="control","Reward"="reward") %>%
    dplyr::mutate(week=as.character(.data$timeId))

  if(max(temp$`Turbining capacity`)>100000){
    temp$"Turbining capacity GWh" <- round(temp$`Turbining capacity`/1000)
    temp$`Turbining capacity` <- NULL
    temp <-  dplyr::relocate(temp,"Turbining capacity GWh", .before = 3)

    p1 <- ggplot2::ggplot(data = temp,ggplot2::aes(x=.data$`Turbining capacity GWh`,.data$Reward, col=week)) +
      ggplot2::geom_line(size=0.5) +
      ggplot2::facet_wrap(ggplot2::vars(.data$mcYear))
  }else{
    p1 <- ggplot2::ggplot(data = temp,ggplot2::aes(x=.data$`Turbining capacity`,.data$Reward, col=week)) +
      ggplot2::geom_line(size=0.5) +
      ggplot2::facet_wrap(ggplot2::vars(.data$mcYear))
    temp <-  dplyr::relocate(temp,"Turbining capacity", .before = 2)

    }

  p1 <- p1+ggplot2::ggtitle(sprintf("Reward week  MC Year %s",paste(as.character(Mc_year),collapse =" ")))+ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))

 if(length(unique(temp$week))>10){
    p1 <- p1+ggplot2::theme(legend.position="none")
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
#' @return a \code{ggplot} object
#' @export

plot_reward_variation_mc <- function(reward_base,week_id,Mc_year)
{
  temp <- reward_base %>%
    dplyr::filter(.data$timeId %in% week_id,.data$mcYear%in%Mc_year) %>%
    dplyr::group_by(.data$mcYear,.data$timeId) %>%
    dplyr::rename(Reward="reward") %>%
    dplyr::mutate(week=as.character(.data$timeId)) %>%
    dplyr::arrange(.data$control) %>%
    dplyr::mutate("Reward transition"=(dplyr::lead(.data$Reward)-.data$Reward)/(dplyr::lead(.data$control)-.data$control)) %>%
    dplyr::rename(`Turbining transistion`="control") %>%
    tidyr::drop_na()

  temp$`Reward transition` <- round(temp$`Reward transition`,digits = 2)


  p1 <- ggplot2::ggplot(data = temp,ggplot2::aes(x=.data$`Turbining transistion`,.data$`Reward transition`, col=week)) +
    ggplot2::geom_line(size=0.5)+
    ggplot2::facet_wrap(ggplot2::vars(.data$mcYear))
  p1 <- p1+ggplot2::ggtitle(sprintf("Reward variation  MC Year %s",paste(as.character(Mc_year),collapse =" ")))+ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
  if(length(unique(temp$week))>10){
    p1 <- p1+ggplot2::theme(legend.position="none")
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
#' @param penalty_low Penalty for the lower rule curve
#' @param penalty_high Penalty for the higher rule curve
#' @param week_number Numeric of length 1. number of the week to plot.
#'
#' @return a \code{ggplot} object
#' @export


plot_Bellman <- function(value_nodes_dt,week_number,penalty_low=10000,penalty_high=10000){

  temp <- value_nodes_dt[value_nodes_dt$weeks %in%week_number]

  temp <- temp %>% dplyr::mutate(value_node=dplyr::case_when(.data$states>.data$level_high ~ .data$value_node - penalty_high*(.data$states-.data$level_high),
                                                             .data$states<.data$level_low ~ .data$value_node  - penalty_low*(.data$level_low-.data$states),
                                                             TRUE ~ .data$value_node ),
                                 states_round_percent=.data$states/max(temp$states)*100) %>%
    dplyr::group_by(.data$weeks) %>%
    dplyr::arrange(.data$weeks,.data$states) %>%
    dplyr::mutate(value_node_dif=.data$value_node-dplyr::lag(.data$value_node)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(is.finite(.data$vu),!is.nan(.data$vu))


  setnames(temp,"value_node","Bellman_Value")
  setnames(temp,"states_round_percent","Reservoir_percent")
  setnames(temp,"vu","Watervalues")
  setnames(temp,"value_node_dif","Gradient_Bellman")
  setnames(temp,"weeks","Week")

  temp <- tidyr::pivot_longer(temp,cols=c(3,8,11),names_to = "Type of value",
                              values_to = "Value")


  p1 <- ggplot2::ggplot(data = temp, ggplot2::aes(.data$`Reservoir_percent` ,
                                                  .data$Value,color=.data$`Week`,
                                                  group=.data$`Week`)) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(dplyr::vars(.data$`Type of value`),scales="free") +
    ggplot2::scale_color_viridis_c(option = "viridis", direction = 1) +
    ggplot2::theme_bw()

  return(p1)
}
