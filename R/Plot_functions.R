
#---------Plot reward variation--------


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


#----------Bellman Plot--------------

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
