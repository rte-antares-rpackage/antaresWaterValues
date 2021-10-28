# The goal of the following lines is only to remove many useless warnings in
# R CMD CHECK: "no visible binding for global variable 'XXX'".
# They come from the use of the data.table syntax and from others syntax (loop,ggplot,..).

utils::globalVariables(
  c("timeId", "area", "hydroStorage", "value_node","i","years","mcYear","time",
    "hydroStorage","tsId","weeks","statesid","level_low","level_high",
    "value_node_dif","states_dif","vu","hstorPMaxHigh","sim_name","value",
    "Reservoir_percent","Bellman_Value","value_node_dif","hydroStorage",
    "Water_Flow","legend","variable","value","area","H. STOR","generation",
    "day","H. LEV","Turbining capacity","Reward","Turbining transistion",
    "Reward transition","states","hstorPMaxHigh","generatingMaxPower",
    "states_percent","i.statesid","i.value_node","i.value_node_dif","i.vu",
    "states_percent","vu_band","addBand","MC_year","nvu","transition",
    "Turbining capacity GWh","filteringOptions","transition_reward",
    "next_bellman_value","max_acc","vu_corr","acc_states","max_acc",
    "reward_base")
)
