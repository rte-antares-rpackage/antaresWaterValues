
#' Plot mean reward variation
#'
#' Plot variation of mean reward functions for different weeks listed in \code{weeks_to_plot}.
#'
#' @inheritParams plot_reward
#'
#' @inherit plot_reward return
#'
#' @export
plot_reward_variation <- function(reward_base,weeks_to_plot)
{
  temp <- reward_base %>%
    dplyr::filter(.data$timeId %in% weeks_to_plot) %>%
    dplyr::group_by(.data$timeId,.data$control) %>%
    dplyr::summarise(Reward=mean(.data$reward),.groups = "drop") %>%
    dplyr::mutate(week=as.character(.data$timeId)) %>%
    dplyr::select(-c("timeId")) %>%
    dplyr::group_by(week) %>%
    dplyr::arrange(.data$week,.data$control) %>%
    dplyr::mutate("Reward transition"=(dplyr::lead(.data$Reward)-.data$Reward)/(dplyr::lead(.data$control)-.data$control)) %>%
    dplyr::rename(`Turbining transition`="control") %>%
    tidyr::drop_na()

  temp$`Reward transition` <- round(temp$`Reward transition`,digits = 2)


  p1 <- ggplot2::ggplot(data = temp,ggplot2::aes(x=.data$`Turbining transition`,.data$`Reward transition`, col=week)) +ggplot2::geom_line(size=0.5)
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

#' Plot mean reward
#'
#' Plot mean reward functions for different weeks listed in \code{weeks_to_plot}.
#'
#' @param reward_base A \code{dplyr::tibble()} containing reward functions. Output \code{reward} from \code{get_Reward()}.
#' @inheritParams plot_Bellman
#'
#' @returns
#' \item{graph}{A \code{ggplot2::ggplot()} object.}
#' \item{table}{A \code{dplyr::tibble()} containing plotted data.}
#' @export
plot_reward <- function(reward_base,weeks_to_plot)
{
  temp <- reward_base %>%
    dplyr::filter(.data$timeId %in% weeks_to_plot) %>%
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


#' Plot reward
#'
#' Plot reward functions for different weeks listed in \code{weeks_to_plot} and different scenarios listed in \code{scenarios_to_plot}.
#'
#' @inheritParams plot_reward
#' @param scenarios_to_plot Vector of integer. Scenarios to plot.
#'
#' @inherit plot_reward return
#'
#' @export
plot_reward_mc <- function(reward_base,weeks_to_plot,scenarios_to_plot)
{
  temp <- reward_base[reward_base$timeId %in% weeks_to_plot&reward_base$mcYear%in%scenarios_to_plot] %>%
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

  p1 <- p1+ggplot2::ggtitle(sprintf("Reward week  MC Year %s",paste(as.character(scenarios_to_plot),collapse =" ")))+ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))

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





#' Plot reward variation
#'
#' Plot variation of reward functions for different weeks listed in \code{weeks_to_plot} and different scenarios listed in \code{scenarios_to_plot}.
#'
#' @inheritParams plot_reward_mc
#'
#' @inherit plot_reward return
#'
#' @export
plot_reward_variation_mc <- function(reward_base,weeks_to_plot,scenarios_to_plot)
{
  temp <- reward_base %>%
    dplyr::filter(.data$timeId %in% weeks_to_plot,.data$mcYear%in%scenarios_to_plot) %>%
    dplyr::group_by(.data$mcYear,.data$timeId) %>%
    dplyr::rename(Reward="reward") %>%
    dplyr::mutate(week=as.character(.data$timeId)) %>%
    dplyr::arrange(.data$control) %>%
    dplyr::mutate("Reward transition"=(dplyr::lead(.data$Reward)-.data$Reward)/(dplyr::lead(.data$control)-.data$control)) %>%
    dplyr::rename(`Turbining transition`="control") %>%
    tidyr::drop_na()

  temp$`Reward transition` <- round(temp$`Reward transition`,digits = 2)


  p1 <- ggplot2::ggplot(data = temp,ggplot2::aes(x=.data$`Turbining transition`,.data$`Reward transition`, col=week)) +
    ggplot2::geom_line(size=0.5)+
    ggplot2::facet_wrap(ggplot2::vars(.data$mcYear))
  p1 <- p1+ggplot2::ggtitle(sprintf("Reward variation  MC Year %s",paste(as.character(scenarios_to_plot),collapse =" ")))+ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
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


#' Plot Bellman and water values
#'
#' Plot Bellman values and water values for differents weeks listed in \code{weeks_to_plot}.
#'
#' @param value_nodes_dt A \code{dplyr::tibble()} containing the Bellman and water values. Output \code{aggregated_results} from \code{Grid_Matrix()}.
#' @param weeks_to_plot Vector of integer. Weeks to plot.
#'
#' @returns A \code{ggplot2::ggplot()} object.
#'
#' @export
plot_Bellman <- function(value_nodes_dt,weeks_to_plot){

  value_nodes_dt <- value_nodes_dt %>%
    dplyr::mutate(week_plot = .data$weeks)
  temp <- value_nodes_dt[value_nodes_dt$week_plot %in%weeks_to_plot]

  temp <- temp %>%
      dplyr::mutate(value_node=dplyr::case_when(.data$states>.data$level_high ~ .data$value_node - .data$penalty_high*(.data$states-.data$level_high),
                                              .data$states<.data$level_low ~ .data$value_node  - .data$penalty_low*(.data$level_low-.data$states),
                                              TRUE ~ .data$value_node))

  temp <- temp %>%
    dplyr::mutate(states_round_percent=.data$states/max(temp$states)*100) %>%
    dplyr::group_by(.data$weeks) %>%
    dplyr::arrange(.data$weeks,.data$states) %>%
    dplyr::mutate(value_node_dif=.data$value_node-dplyr::lag(.data$value_node),
                  weeks=as.character(.data$week_plot)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(is.finite(.data$vu),!is.nan(.data$vu))


  setnames(temp,"value_node","Bellman_Value")
  setnames(temp,"states_round_percent","Reservoir_percent")
  setnames(temp,"vu","Watervalues")
  setnames(temp,"value_node_dif","Gradient_Bellman")
  setnames(temp,"weeks","Week")


  temp <- tidyr::pivot_longer(temp,cols=c("Bellman_Value","Watervalues"),#,"Gradient_Bellman"
                              names_to = "Type of value",
                              values_to = "Value")


  p1 <- ggplot2::ggplot(data = temp, ggplot2::aes(.data$`Reservoir_percent` ,
                                                  .data$Value,color=.data$`Week`,
                                                  group=.data$`Week`)) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(dplyr::vars(.data$`Type of value`),scales="free") +
    ggplot2::theme_bw()
  if(length(unique(temp$Week))>10){
    p1 <- p1+ggplot2::theme(legend.position="none")
  }

  return(p1)
}
